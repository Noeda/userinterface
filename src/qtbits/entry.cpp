#include "haskell_event_glue.h"
#include <QtWidgets/QApplication>

extern "C" {
    int run_qt( int argc, char** argv );
    void post_delete_event( QObject* object );
    void application_quit( void );
}

static int should_quit = 0;

int run_qt( int argc, char** argv )
{
    should_quit = 0;

    HaskellEventGlue heg;
    QApplication app( argc, argv );
    QObject::connect( &app, SIGNAL(aboutToQuit())
                    , &heg, SLOT(aboutToQuit()) );
    heg.ready();
    if ( should_quit ) {
        return -1;
    }
    return app.exec();
}

void post_delete_event( QObject* object )
{
    object->deleteLater();
}

void application_quit( void )
{
    should_quit = 1;
    QApplication::quit();
}

