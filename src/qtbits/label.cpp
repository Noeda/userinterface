#include <QLabel>
#include <cstdio>

// Make this agree on the haskell side
enum Interpretation
    { PlainText = 0
    , RichText = 1 };

extern "C" {
    QLabel* create_label(void);
    void set_label_buddy( QLabel* label, QWidget* buddy );
    void set_label_text( QLabel* label, QString* str );
    QString* get_label_text( QLabel* label );
    void set_label_interpretation( QLabel* label, int interpretation );
    int get_label_interpretation( QLabel* label );
}

QLabel* create_label(void)
{
    QLabel* label = new QLabel();
    label->setTextFormat( Qt::PlainText );
    return label;
}

void set_label_text( QLabel* label, QString* str )
{
    label->setText( *str );
}

QString* get_label_text( QLabel* label )
{
    return new QString( label->text() );
}

void set_label_interpretation( QLabel* label, int interpretation )
{
    switch (interpretation)
    {
        case PlainText:
            label->setTextFormat( Qt::PlainText );
            return;
        case RichText:
            label->setTextFormat( Qt::RichText );
            return;
        default:
            fprintf( stderr
                   , "(warning) "
                     "set_label_interpretation: interpretation value "
                     "is nonsensical (%d)\n", interpretation );
            return;
    }
}

int get_label_interpretation( QLabel* label )
{
    Qt::TextFormat tf = label->textFormat();
    switch (tf)
    {
        case Qt::PlainText:
            return PlainText;
        case Qt::RichText:
            return RichText;
        default:
            return PlainText;
    }
}

void set_label_buddy( QLabel* label, QWidget* buddy )
{
    label->setBuddy( buddy );
}


