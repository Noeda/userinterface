#include "haskell_action.h"
#include <QMainWindow>
#include <QMdiArea>
#include <QMdiSubWindow>

extern "C" {
    QMainWindow* create_main_window( void );
    void set_menubar( QMainWindow*, QMenuBar* );

    void show_widget( QWidget* );

    void set_central_widget( QMainWindow*, QWidget* );
    QWidget* take_central_widget( QMainWindow* );

    QMdiArea* create_mdiarea( void );
    QMdiSubWindow* add_subwindow( QMdiArea* mdi
                                , QWidget* widget );
};

QMainWindow* create_main_window( void )
{
    QMainWindow* w = new QMainWindow( NULL, Qt::Window );
    return w;
}

void show_widget( QWidget* widget )
{
    widget->show();
}

QMdiArea* create_mdiarea( void )
{
    return new QMdiArea;
}

void set_menubar( QMainWindow* window, QMenuBar* menubar )
{
    window->setMenuBar( menubar );
}

void set_central_widget( QMainWindow* window, QWidget* widget )
{
    window->setCentralWidget( widget );
}

QWidget* take_central_widget( QMainWindow* window )
{
    if ( window->centralWidget() ) {
        return window->takeCentralWidget();
    } else {
        return NULL;
    }
}

QMdiSubWindow* add_subwindow( QMdiArea* mdi
                            , QWidget* widget )
{
    QMdiSubWindow* sub = mdi->addSubWindow( widget );
    QObject::connect( (QObject*) widget, SIGNAL(destroyed())
                    , (QObject*) sub, SLOT(deleteLater()) );
    sub->show();
    return sub;
}

