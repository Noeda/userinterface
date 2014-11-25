#include "haskell_action.h"
#include <QDockWidget>
#include <QMainWindow>

extern "C" {
    QDockWidget* create_dockwidget( QMainWindow*, QWidget*
                                  , void (*destroy_callback)(void) );
}

QDockWidget* create_dockwidget( QMainWindow* mainwindow, QWidget* child
                              , void (*destroy_callback)(void) )
{
    QDockWidget* dock_widget = new QDockWidget( (QWidget*) mainwindow );
    child->setParent( dock_widget );
    dock_widget->show();
    mainwindow->addDockWidget( Qt::LeftDockWidgetArea, dock_widget );

    HaskellAction* ha = new HaskellAction( (QObject*) dock_widget
                                         , destroy_callback );
    QObject::connect( (QObject*) dock_widget, SIGNAL(destroyed())
                    , ha, SLOT(trigger()) );
    QObject::connect( (QObject*) child, SIGNAL(destroyed())
                    , (QObject*) dock_widget, SLOT(deleteLater()) );
    return dock_widget;
}

