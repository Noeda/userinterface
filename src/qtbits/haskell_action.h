#ifndef qtbits_haskell_action_h
#define qtbits_haskell_action_h

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

class IntHaskellAction : public QObject
{
    Q_OBJECT
private:
    void (*haskellTrigger)(int);

public:
    IntHaskellAction( QObject* parent
                    , void (*hsTrigger)(int) );
    virtual ~IntHaskellAction();

public slots:
    void trigger(int);
    void trigger(bool);
};

#endif

