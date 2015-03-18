namespace FSharp.Data.Xml

open System.Xml

[<AutoOpen>]
module XmlActivePatterns = 

    /// Matches any XML document node
    let (|XmlDocument|_|) (node : XmlNode) =
        match node with
        | n when n.NodeType = XmlNodeType.Document -> Some(node :?> XmlDocument)
        | _ -> None

    /// Matches any XML element node
    let (|XmlElement|_|) (node : XmlNode) =
        match node with
        | n when n.NodeType = XmlNodeType.Element -> Some(node :?> XmlElement)
        | _ -> None

    /// Matches any XML element node with the specified name
    let (|XmlElem|_|) (name : string) (node : XmlNode) =
        match node with
        | n when n.NodeType = XmlNodeType.Element && n.Name = name -> Some(node :?> XmlElement)
        | _ -> None

    /// Matches any XML attribute node
    let (|XmlAttribute|_|) (node : XmlNode) =
        match node with
        | n when n.NodeType = XmlNodeType.Attribute -> Some(node :?> XmlAttribute)
        | _ -> None

    /// Matches any XML attribute node with the specified name
    let (|XmlAttr|_|) (name : string) (node : XmlNode) =
        match node with
        | n when n.NodeType = XmlNodeType.Attribute && n.Name = name -> Some(node :?> XmlAttribute)
        | _ -> None