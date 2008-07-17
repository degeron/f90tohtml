#include <params.h>
      integer function myhandler( sig, code, context )
      integer sig, code, context(5)
      myhandler = abort()
      return
      end
