#include "haskell_action.h"
#include <QMainWindow>
#include <QMenuBar>
#include <cassert>
#include <cstdio>

extern "C" {
    // exports
    QMenuBar* create_menubar( void );
    QMenu* create_menu( QString* str );
    void add_menubar_menu( QMenuBar* menubar, QMenu* menu );
    void add_menu_action( QMenu* menu
                        , QString* name
                        , void (*hsTrigger)(void) );

    // imports
    void freeHaskellFunPtr( void* funptr );
}

QMenuBar* create_menubar( void )
{
    return new QMenuBar;
}

QMenu* create_menu( QString* str )
{
    return new QMenu( *str );
}

void add_menubar_menu( QMenuBar* menubar, QMenu* menu )
{
    menubar->addMenu( menu );
}

void add_menu_action( QMenu* menu
                    , QString* name
                    , void (*hsTrigger)(void) )
{
    HaskellAction* ha = new HaskellAction( menu, hsTrigger );
    menu->addAction( *name, ha, SLOT(trigger()) );
}

HaskellAction::HaskellAction( QObject* parent, void (*hsTrigger)(void) )
                            : QObject(parent)
                            , haskellTrigger(hsTrigger)
{
}

HaskellAction::~HaskellAction()
{
    freeHaskellFunPtr((void*) haskellTrigger);
}

void HaskellAction::trigger()
{
    haskellTrigger();
}

void HaskellAction::trigger_qobject( QObject* )
{
    haskellTrigger();
}

