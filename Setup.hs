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
        return lbi

   ,buildHook = \pd lbi uh bf -> do
        let qmake_program = fromJust $ snd $ fromJust $ findQmake lbi
            make_program = fromJust $ snd $ fromJust $ findMake lbi
        old_path <- getCurrentDirectory
        let qt_build_dir = buildDir lbi ++ "/qtbits"
        createDirectoryIfMissing True qt_build_dir
        setCurrentDirectory "src/qtbits"
        runProgram normal qmake_program ["qtbits.pro", "-o", "../../" ++ qt_build_dir ++ "/Makefile"]
        setCurrentDirectory old_path
        setCurrentDirectory qt_build_dir
        runProgram normal make_program []
        setCurrentDirectory old_path

        -- link to the built Qt library
        let pd' = linkQtTo "bequel" (old_path ++ "/" ++ buildDir lbi) pd

        buildHook simpleUserHooks pd' lbi uh bf
    }

linkQtTo :: String -> FilePath -> PackageDescription -> PackageDescription
linkQtTo name dist_dir pd =
    pd { library = fmap addLinking' (library pd) }
  where
    addLinking' thing = thing { libBuildInfo = addLinking'' (libBuildInfo thing) }

    addLinking'' bi = bi
        { extraLibs = extraLibs bi++["qtbits"]
        , extraLibDirs = extraLibDirs bi ++ [dist_dir ++ "/qtbits"] }

