#ifndef haskell_event_glue_h
#define haskell_event_glue_h

#include <QObject>
#include <QEvent>

class HaskellEventGlue : public QObject
{
    Q_OBJECT
public:
    ~HaskellEventGlue();
    void ready();

protected:
    bool eventFilter( QObject* obj, QEvent* event );

private slots:
    void aboutToQuit();
};

#endif

