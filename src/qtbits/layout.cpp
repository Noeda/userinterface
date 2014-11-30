#include <QBoxLayout>
#include <QFormLayout>
#include <QStackedLayout>
#include <QGridLayout>
#include <QLabel>

extern "C" {
    void set_layout_alignment( QLayout*, int );
    void set_layout_margins( QLayout*, int, int, int, int );
    void get_layout_margins( QLayout*, int*, int*, int*, int* );
    void set_layout_spacing( QLayout*, int );
    int get_layout_spacing( QLayout* );
    void set_layout_size_constraint( QLayout*, int );
    int get_layout_size_constraint( QLayout* );

    QBoxLayout* create_box_layout( void );
    void add_widget_to_box_layout( QBoxLayout*, QWidget*, int, int );
    void set_box_layout_direction( QBoxLayout*, int );
    int get_box_layout_direction( QBoxLayout* );
    void add_box_strut( QBoxLayout*, int );
    void add_box_spacing( QBoxLayout*, int );
    void add_box_stretch( QBoxLayout*, int );

    QFormLayout* create_form_layout( void );
    void add_form_row( QFormLayout*, QLabel*, QWidget* );

    QStackedLayout* create_stacked_layout( void );
    int add_stacked_page( QStackedLayout*, QWidget* );
    void set_stacked_page( QStackedLayout* layout, int page );
    int get_stacked_page( QStackedLayout* layout );

    QGridLayout* create_grid_layout( void );
    void set_grid_corner( QGridLayout*, int );
    int get_grid_corner( QGridLayout* );
    void add_widget_to_grid_layout( QGridLayout*, QWidget*, int, int );
}

void set_layout_alignment( QLayout* layout, int align )
{
    layout->setAlignment( layout, (Qt::Alignment) align );
}

void set_layout_margins( QLayout* layout, int l, int t, int r, int b )
{
    layout->setContentsMargins( l, t, r, b );
}

void get_layout_margins( QLayout* layout, int* l, int* t, int* r, int* b )
{
    layout->getContentsMargins( l, t, r, b );
}

void set_layout_spacing( QLayout* layout, int spacing )
{
    layout->setSpacing( spacing );
}

int get_layout_spacing( QLayout* layout )
{
    return layout->spacing();
}

QBoxLayout* create_box_layout( void )
{
    return new QBoxLayout( QBoxLayout::LeftToRight );
}

void add_widget_to_box_layout( QBoxLayout* layout, QWidget* widget, int align, int stretching )
{
    layout->addWidget( widget, stretching, (Qt::Alignment) align );
}

void set_box_layout_direction( QBoxLayout* layout, int dir )
{
    layout->setDirection( (QBoxLayout::Direction) dir );
}

int get_box_layout_direction( QBoxLayout* layout )
{
    return layout->direction( );
}

void add_box_strut( QBoxLayout* layout, int strut )
{
    layout->addStrut( strut );
}

void add_box_spacing( QBoxLayout* layout, int spacing )
{
    layout->addSpacing( spacing );
}

void add_box_stretch( QBoxLayout* layout, int stretching )
{
    layout->addStretch( stretching );
}

QFormLayout* create_form_layout( void )
{
    return new QFormLayout;
}

QStackedLayout* create_stacked_layout( void )
{
    return new QStackedLayout;
}

QGridLayout* create_grid_layout( void )
{
    return new QGridLayout;
}

void add_form_row( QFormLayout* layout, QLabel* label, QWidget* field )
{
    layout->addRow( label, field );
}

int add_stacked_page( QStackedLayout* layout, QWidget* widget )
{
    return layout->addWidget( widget );
}

void set_stacked_page( QStackedLayout* layout, int page )
{
    layout->setCurrentIndex( page );
}

int get_stacked_page( QStackedLayout* layout )
{
    return layout->currentIndex();
}

void add_widget_to_grid_layout( QGridLayout* layout
                              , QWidget* widget
                              , int row
                              , int column )
{
    layout->addWidget( widget, row, column );
}

void set_layout_size_constraint( QLayout* layout, int size )
{
    layout->setSizeConstraint( (QLayout::SizeConstraint) size );
}

int get_layout_size_constraint( QLayout* layout )
{
    return (int) layout->sizeConstraint();
}

void set_grid_corner( QGridLayout* layout, int corner )
{
    layout->setOriginCorner( (Qt::Corner) corner );
}

int get_grid_corner( QGridLayout* layout )
{
    return (int) layout->originCorner();
}


