#include <QMainWindow>
#include <QMdiArea>
#include <cstdio>

extern "C" {
    QMainWindow* create_main_window( void );
    void set_menubar( QMainWindow*, QMenuBar* );
};

QMainWindow* create_main_window( void )
{
    QMainWindow* w = new QMainWindow( NULL, Qt::Window );
    QWidget* central = new QMdiArea;
    w->setCentralWidget( central );
    w->show();
    return w;
}

void set_menubar( QMainWindow* window, QMenuBar* menubar )
{
    window->setMenuBar( menubar );
}

