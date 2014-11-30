#include "haskell_action.h"
#include <QGroupBox>

extern "C" {
    QGroupBox* create_groupbox( void (*)( int ) );
    void set_groupbox_title( QGroupBox*, QString* );
    QString* get_groupbox_title( QGroupBox* );
    void set_groupbox_flat( QGroupBox*, int );
    int get_groupbox_flat( QGroupBox* );
    void set_groupbox_checked( QGroupBox*, int );
    int get_groupbox_checked( QGroupBox* );
    void set_groupbox_checkable( QGroupBox*, int );
    int get_groupbox_checkable( QGroupBox* );
    void set_groupbox_alignment( QGroupBox*, int );
    int get_groupbox_alignment( QGroupBox* );

    // imports
    void freeHaskellFunPtr( void* funptr );
}

QGroupBox* create_groupbox( void (*checked)( int ) )
{
    QGroupBox* box = new QGroupBox;
    IntHaskellAction* ha = new IntHaskellAction( box, checked );
    QObject::connect( box, SIGNAL( toggled(bool) )
                    , ha, SLOT( trigger(bool) ) );
    return box;
}

void set_groupbox_title( QGroupBox* box, QString* str )
{
    box->setTitle( *str );
}

QString* get_groupbox_title( QGroupBox* box )
{
    return new QString( box->title() );
}

void set_groupbox_flat( QGroupBox* box, int flat )
{
    box->setFlat( flat );
}

int get_groupbox_flat( QGroupBox* box )
{
    return box->isFlat();
}

void set_groupbox_checked( QGroupBox* box, int checked )
{
    box->setChecked( checked );
}

int get_groupbox_checked( QGroupBox* box )
{
    return box->isChecked();
}

void set_groupbox_checkable( QGroupBox* box, int checkable )
{
    box->setCheckable( checkable );
}

int get_groupbox_checkable( QGroupBox* box )
{
    return box->isCheckable();
}

void set_groupbox_alignment( QGroupBox* box, int alignment )
{
    box->setChecked( (Qt::Alignment) alignment );
}

int get_groupbox_alignment( QGroupBox* box )
{
    return (int) box->alignment();
}

IntHaskellAction::IntHaskellAction( QObject* parent, void (*hsTrigger)(int) )
                                  : QObject(parent)
                                  , haskellTrigger(hsTrigger)
{
}

IntHaskellAction::~IntHaskellAction()
{
    freeHaskellFunPtr((void*) haskellTrigger);
}

void IntHaskellAction::trigger(bool value)
{
    haskellTrigger((int) value);
}

void IntHaskellAction::trigger(int value)
{
    haskellTrigger(value);
}

