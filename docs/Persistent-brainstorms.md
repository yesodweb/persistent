# Persistent brainstorms

# Simplified monad stack handling

An issue I (Michael) have run into is complications with complex monad stacks. This is unfortunate, since all current backends could essentially drop their specialized monads and just expect a `Connection` value to be passed in to all functions. (This isn't true right now due to how the mongoDB package works, but that could be fixed upstream.)

Ideally, I'd like all functions to have core signatures such as:

    getImpl :: Key entity -> backend -> IO (Maybe entity)

And then have a monadic interface which is essentially just a `ReaderT` provided for convenience.



# Triggers

Code that executes on database changes.

Ideally triggers would be a transformer layer, but an easier route may be to register them in IO (TCache does this).

Thoughts on this are still raw

What if you wanted to have an audit log of changes? Right now you would need to create [separate functions](http://devblog.soostone.com/posts/2013-05-10-snaplet-actionlog.html) such as loggedInsert. But then you need to have control over every insert and change it to loggedInsert. Loggging database changes could be handled in the background, even by a separate process via a queue. There are different types of triggers.

* can be handled by database triggers
* application triggers: the database doesn't have enough information to handle them

If the database can handle a trigger, one still might want a trigger to be generated like a migration so that triggers can be verified and possibly made portable across databases. There is a library with support for writing postgres triggers in a persistent schema.

Another thing to think about is if this concept ties in to the concept of auto-updating the client-side when data changes. If triggers exist only in an application, then we run into trouble if there is a second application that writes to the database. For example, it is easy enough to automatically send an e-mail from  our Haskell application when a model changes. If we have multiple writers then we need our Haskell application to be automatically notified when the model changes.

So the simple way to deal with triggers that can't be performed by the database is to put all triggers in a single application and tie them to the writes done by that application. The correct but hard way is to have the application triggers respond to changes in the database. In some cases both approaches may be needed: an application may want to ignore changes performed by other applications (allow admin access for data editing).
