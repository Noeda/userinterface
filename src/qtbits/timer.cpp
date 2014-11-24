#include "menubar.h"
#include <QTimer>

extern "C" {
    QTimer* create_timer( int, int, void (*)(void) );
}

QTimer* create_timer( int oneshot, int milliseconds, void (*callback)(void) )
{
    QTimer* t = new QTimer;
    HaskellAction* ha = new HaskellAction( t, callback );
    QObject::connect( t, SIGNAL(timeout()), ha, SLOT(trigger()) );
    t->setSingleShot( oneshot );
    t->start( milliseconds );
    return t;
}

