# Ch26
The general pattern with MonadTrans instances demonstrated
by MaybeT is that you’re usually going to lift the injection of the
known structure over some Monad. Injection of structure usually means return,
That transforms an "m a" into "m (T a)" where capital "T" is some concrete
type you’re lifting the "m a" into. Then to cap it all off, you use the data
constructor for your monad transformer, and the value is now lifted into
the larger context.
