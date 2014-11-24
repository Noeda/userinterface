#include <QString>

extern "C" {
    QString* create_qstring( const char* utf8, int utf8_len );
    void free_qstring( QString* str );
}

QString* create_qstring( const char* utf8, int utf8_len )
{
    return new QString(QString::fromUtf8( utf8, utf8_len ));
}

void free_qstring( QString* str )
{
    delete str;
}

