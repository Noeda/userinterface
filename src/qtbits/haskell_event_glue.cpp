#include "haskell_event_glue.h"

extern "C" {
    /* Haskell functions */
    extern int eventCallback( QObject* obj, QEvent* event );
    extern void aboutToQuitMsg( void );
    extern void readyMsg( void );
}

HaskellEventGlue::~HaskellEventGlue()
{
}

bool HaskellEventGlue::eventFilter( QObject* obj, QEvent* event )
{
    return false;
}

void HaskellEventGlue::aboutToQuit()
{
    aboutToQuitMsg();
}

void HaskellEventGlue::ready()
{
    readyMsg();
}

