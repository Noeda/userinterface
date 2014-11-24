#include "haskell_event_glue.h"
#include <QtWidgets/QApplication>

extern "C" {
    int run_qt( int argc, char** argv );
    void post_delete_event( QObject* object );
}

int run_qt( int argc, char** argv )
{
    HaskellEventGlue heg;
    QApplication app( argc, argv );
    QObject::connect( &app, SIGNAL(aboutToQuit())
                    , &heg, SLOT(aboutToQuit()) );
    heg.ready();
    int result = app.exec();
}

void post_delete_event( QObject* object )
{
    object->deleteLater();
}

