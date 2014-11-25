#include <QWidget>

extern "C" {
    void set_widget_title( QWidget* widget, QString* str );
    QString* get_widget_title( QWidget* widget );
}

void set_widget_title( QWidget* widget, QString* str )
{
    widget->setWindowTitle( *str );
}

QString* get_widget_title( QWidget* widget )
{
    return new QString(widget->windowTitle());
}

