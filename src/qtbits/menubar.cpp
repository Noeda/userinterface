#include "haskell_action.h"
#include <QMainWindow>
#include <QMenuBar>
#include <cassert>
#include <cstdio>

extern "C" {
    // exports
    QMenuBar* create_menubar( void );
    QMenu* add_menubar_menu( QMenuBar* menubar, QString* title );
    QMenu* add_menu_menu( QMenu* menu_parent, QString* title );
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

QMenu* add_menubar_menu( QMenuBar* menubar, QString* title )
{
    return menubar->addMenu( *title );
}

QMenu* add_menu_menu( QMenu* menu_parent, QString* title )
{
    return menu_parent->addMenu( *title );
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

