#include "haskell_event_glue.h"
#include "haskell_action.h"
#include <QtWidgets/QApplication>
#include <QTimer>

extern "C" {
    int run_qt( int argc, char** argv, void (*tick)(void) );
    void post_delete_event( QObject* object );
    void application_quit( void );

    void link_destroy_event( QObject* object, void (*trigger)(void) );

    int number_of_qobject_children( const QObject* object );
    void get_qobject_children( QObject* parent, QObject** children );
    QObject* get_qobject_parent( QObject* object );
}

static int should_quit = 0;

int run_qt( int argc, char** argv, void (*tick)(void) )
{
    should_quit = 0;

    HaskellEventGlue heg;
    QApplication app( argc, argv );
    QObject::connect( &app, SIGNAL(aboutToQuit())
                    , &heg, SLOT(aboutToQuit()) );

    QTimer timer;
    timer.setSingleShot( false );
    timer.setInterval( 5000 );

    HaskellAction ha( NULL, tick );
    QObject::connect( &timer, SIGNAL(timeout())
                    , &ha, SLOT(trigger()) );
    timer.start();

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

int number_of_qobject_children( const QObject* object )
{
    const QObjectList &lst = object->children();
    return lst.size();
}

void get_qobject_children( QObject* parent, QObject** children )
{
    const QObjectList &lst = parent->children();
    QList<QObject*>::const_iterator ci;
    for ( ci = lst.begin(); ci != lst.end(); ++ci ) {
        (*children) = *ci;
        ++children;
    }
}

QObject* get_qobject_parent( QObject* object )
{
    return object->parent();
}

void link_destroy_event( QObject* object, void (*trigger)(void) )
{
    HaskellAction* ha = new HaskellAction( object, trigger );
    QObject::connect( object, SIGNAL(destroyed())
                    , ha, SLOT(trigger()) );
}

