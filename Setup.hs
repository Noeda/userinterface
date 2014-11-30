import Data.List ( find )
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Verbosity
import System.Directory
import System.Process

-- This custom configuration script finds qmake and builds the Qt parts of our
-- application.

qmake = "qmake"
make = "make"

findP :: String -> LocalBuildInfo -> Maybe (Program, Maybe ConfiguredProgram)
findP prog lbi = find (\(p, c) -> programName p == prog)
                      (knownPrograms (withPrograms lbi))

findQmake :: LocalBuildInfo -> Maybe (Program, Maybe ConfiguredProgram)
findQmake = findP "qmake"

findMake :: LocalBuildInfo -> Maybe (Program, Maybe ConfiguredProgram)
findMake = findP "make"

main = defaultMainWithHooks simpleUserHooks {
    -- We need qmake, find it!
    hookedPrograms = hookedPrograms simpleUserHooks ++
                     [(simpleProgram qmake)
                      { programPostConf = \_ conf -> return $ conf {
                          programDefaultArgs =
                              "-makefile":programDefaultArgs conf } }
                     ,simpleProgram make]

    -- check that we have the qmake
   ,confHook = \args cflags -> do
        lbi <- confHook simpleUserHooks args cflags
        let guard_program finder name = case finder lbi of
                                Just (_, Just _) -> return ()
                                _ -> error $ "Program not found: " ++ name
        guard_program findQmake "qmake"
        guard_program findMake "make"

        -- We have to build the qtbits in *configuration* phase or otherwise
        -- the library won't be picked up by any programs that use this
        -- library.
        --
        -- Things I tried:
        --
        --    Just find qmake/make and then build qtbits in buildHook -->
        --    libraries using this library won't pickup qtbits and fail
        --
        --    Same as above but add qtbits to confHook as extra library -->
        --    cabal configure fails for this package.
        --
        --    Can we add an extra library in confHook but make it not fail if
        --    it's missing? I don't know how so we are stuck with this hack.
        path <- getCurrentDirectory
        let qt_build_dir = path ++ "/" ++ buildDir lbi
            new_lbi = lbi { localPkgDescr =
                            linkQtTo (qt_build_dir)
                                     (localPkgDescr lbi) }

        let qmake_program = fromJust $ snd $ fromJust $ findQmake new_lbi
            make_program = fromJust $ snd $ fromJust $ findMake new_lbi
        old_path <- getCurrentDirectory
        let qt_build_dir = buildDir new_lbi ++ "/qtbits"
        createDirectoryIfMissing True qt_build_dir
        setCurrentDirectory "src/qtbits"
        runProgram normal qmake_program ["qtbits.pro", "-o", "../../" ++ qt_build_dir ++ "/Makefile"]
        setCurrentDirectory old_path
        setCurrentDirectory qt_build_dir
        runProgram normal make_program []
        setCurrentDirectory old_path

        return new_lbi
    }

linkQtTo :: FilePath -> PackageDescription -> PackageDescription
linkQtTo dist_dir pd =
    pd { library = fmap addLinking' (library pd) }
  where
    addLinking' thing = thing { libBuildInfo = addLinking'' (libBuildInfo thing) }

    addLinking'' bi = bi
        { extraLibs = extraLibs bi++["qtbits"]
        , extraLibDirs = extraLibDirs bi ++ [dist_dir ++ "/qtbits"] }

