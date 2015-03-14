namespace FSharp.Data.Xml

open System.IO
open System.Xml

module Xml = 

    /// Loads an XML document from a stream
    let ofStream (stream : Stream) =
        Argument.validateNotNull stream "stream"
        let document = XmlDocument()
        document.Load(stream)
        document