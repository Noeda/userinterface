#include <QTextEdit>
#include "haskell_action.h"

extern "C" {
    QTextEdit* create_textedit( void (*)(void) );
    void set_textedit_readable( QTextEdit* te, int readonly );
    int get_textedit_readable( QTextEdit* te );
    void set_textedit_html( QTextEdit* te, QString* str );
    QString* get_textedit_html( QTextEdit* te );
}

QTextEdit* create_textedit( void (*callback)(void) )
{
    QTextEdit* te = new QTextEdit;
    HaskellAction* ha = new HaskellAction( te, callback );
    QObject::connect( te, SIGNAL(textChanged()), ha, SLOT(trigger()) );
    return te;
}

void set_textedit_readable( QTextEdit* te, int readonly )
{
    te->setReadOnly( readonly );
}

int get_textedit_readable( QTextEdit* te )
{
    return te->isReadOnly();
}

void set_textedit_html( QTextEdit* te, QString* str )
{
    te->setHtml( *str );
}

QString* get_textedit_html( QTextEdit* te )
{
    QString* str = new QString(te->toHtml());
    return str;
}

