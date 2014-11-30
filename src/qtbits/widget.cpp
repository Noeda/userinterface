#include <QWidget>

extern "C" {
    void set_widget_title( QWidget* widget, QString* str );
    QString* get_widget_title( QWidget* widget );

    void set_widget_fixed_size( QWidget*, int, int );
    void set_widget_layout( QWidget* widget, QLayout* layout );
    QLayout* get_widget_layout( QWidget* widget );
}

void set_widget_title( QWidget* widget, QString* str )
{
    widget->setWindowTitle( *str );
}

QString* get_widget_title( QWidget* widget )
{
    return new QString(widget->windowTitle());
}

void set_widget_fixed_size( QWidget* widget, int w, int h )
{
    if ( w == -1 || h == -1 ) {
        widget->setFixedSize( QSize(QWIDGETSIZE_MAX, QWIDGETSIZE_MAX) );
    } else {
        widget->setFixedSize( w, h );
    }
}

void set_widget_layout( QWidget* widget, QLayout* layout )
{
    widget->setLayout( layout );
}

QLayout* get_widget_layout( QWidget* widget )
{
    return widget->layout();
}


