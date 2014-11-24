#ifndef qtbits_menubar_h
#define qtbits_menubar_h

#include <QObject>
#include <cstdio>

class HaskellAction : public QObject
{
    Q_OBJECT
private:
    void (*haskellTrigger)(void);

public:
    HaskellAction( QObject* parent
                 , void (*hsTrigger)(void) );
    virtual ~HaskellAction();

public slots:
    void trigger();
};

#endif

