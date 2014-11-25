#include <QString>

extern "C" {
    QString* create_qstring( const char* utf8, int utf8_len );
    void free_qstring( QString* str );
    int get_qstring_utf16_size( const QString* str );
    const char* get_qstring_as_utf16( const QString* str );
}

QString* create_qstring( const char* utf8, int utf8_len )
{
    return new QString(QString::fromUtf8( utf8, utf8_len ));
}

void free_qstring( QString* str )
{
    delete str;
}

int get_qstring_utf16_size( const QString* str )
{
    return str->size();
}

const char* get_qstring_as_utf16( const QString* str )
{
    return (const char*) str->utf16();
}

