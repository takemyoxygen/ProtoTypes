namespace ProtoTypes.Core

open Froto.Core

// Eventually, this class might be replaced by Froto.Core.Encoding.MessageBase, but so far
// this interface looks simpler and satisfies all needs.

/// Base class for types generated from proto messages.
[<AbstractClass>]
type Message() as this =
 
    let mutable size = lazy (
        let buffer = NullWriteBuffer()
        this.Serialize buffer
        buffer.Length
    )
 
    member this.SerializedLength = size.Value
    
    abstract Serialize: ZeroCopyBuffer -> unit
    
    abstract ReadFrom: ZeroCopyBuffer -> unit

/// Simple implementation of Message class that does nothing useful
/// Basically, this class is needed only for type inference within quotations, because it satisfies requirements
/// to be inherited from Message and to have constructor without parameters
type internal Dummy() = 
    inherit Message()
    
    override this.Serialize(buffer) = ()
    override this.ReadFrom(buffer) = ()