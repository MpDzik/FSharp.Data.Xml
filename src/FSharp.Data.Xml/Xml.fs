namespace FSharp.Data.Xml

open System.Collections.Generic
open System.IO
open System.Linq
open System.Xml

module Xml = 

    /// Caches XmlNamespaceManager objects for XML documents
    let nsMgrs = Dictionary<XmlNameTable, XmlNamespaceManager>()

    /// Loads an XML document from a stream
    let ofStream (stream : Stream) =
        Argument.validateNotNull stream "stream"
        let document = XmlDocument()
        document.Load(stream)
        document

    /// Loads an XML document from a string
    let ofString (xml : string) =
        Argument.validateNotNull xml "xml"
        let document = XmlDocument()
        document.LoadXml(xml)
        document

    /// Creates an XML namespace manager for the specified XML name table
    let private newNsMgr nameTable = 
        let nsmgr = XmlNamespaceManager(nameTable)
        nsMgrs.[nameTable] <- nsmgr
        nsmgr

    /// Gets the XML namespace manager for the specified XML document
    let nsmgr (document : XmlDocument) =
        Argument.validateNotNull document "document"
        match nsMgrs.TryGetValue document.NameTable with
        | true, nsmgr -> nsmgr
        | _ -> newNsMgr document.NameTable

    /// Gets the root element of the specified XML document
    let root (document : XmlDocument) =
        Argument.validateNotNull document "document"
        let root = Seq.cast<XmlNode>(document.ChildNodes) |> Seq.find (fun n -> n :? XmlElement)
        root :?> XmlElement

    /// Tries to get the root element of the specified XML document
    let tryRoot (document : XmlDocument) =
        if document <> null then
            match Seq.cast<XmlNode>(document.ChildNodes) |> Seq.tryFind (fun n -> n :? XmlElement) with
            | Some root -> Some (root :?> XmlElement)
            | None -> None
        else None