namespace FSharp.Data.Xml

open System.Collections.Generic
open System.IO
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

    /// Performs an XPath query on an XML node and returns the result nodes
    let query xpath (node : XmlNode) =
        Argument.validateNotNull xpath "xpath"
        Argument.validateNotNull node "node"
        let result = 
            match node with
            | :? XmlDocument as document -> node.SelectNodes(xpath, (nsmgr document))
            | n when n.OwnerDocument <> null -> node.SelectNodes(xpath, (nsmgr node.OwnerDocument))
            | _ -> node.SelectNodes(xpath)
        Seq.cast<XmlNode>(result)

    /// Performs an XPath query on an XML node and returns a single result node
    let querySingle xpath (node : XmlNode) =
        Argument.validateNotNull xpath "xpath"
        Argument.validateNotNull node "node"
        match node with
        | :? XmlDocument as document -> node.SelectSingleNode(xpath, (nsmgr document))
        | n when n.OwnerDocument <> null -> node.SelectSingleNode(xpath, (nsmgr node.OwnerDocument))
        | _ -> node.SelectSingleNode(xpath)

    /// Performs an XPath query on an XML node and returns a single result node, returns None if no results were
    /// returned from the query
    let tryQuerySingle xpath (node : XmlNode) =
        if (xpath <> null) && (node <> null) then
            let result = 
                match node with
                | :? XmlDocument as document -> node.SelectSingleNode(xpath, (nsmgr document))
                | n when n.OwnerDocument <> null -> node.SelectSingleNode(xpath, (nsmgr node.OwnerDocument))
                | _ -> node.SelectSingleNode(xpath)
            if result <> null then Some result
            else None
        else None

    let private importNode (document : XmlDocument) (node : XmlNode) =
        if node.OwnerDocument <> document then document.ImportNode(node, true)
        else node

    let private insertGeneric (node : XmlNode) (newNodes : seq<XmlNode>) (insertOp : XmlNode -> XmlNode -> XmlNode -> XmlNode) =
        let rec insertRec (node : XmlNode) (newNodes : XmlNode list) appended =
            match newNodes with
            | [] -> appended
            | h :: t -> 
                let newNode = h |> importNode node.OwnerDocument
                insertRec (node.ParentNode |> insertOp newNode node) t (newNode :: appended)
        Argument.validateNotNull node "node"
        Argument.validateNotNull newNodes "newNodes"
        (insertRec node (List.ofSeq newNodes) []) |> List.rev

    /// Inserts the specified XML node after the specified node, returns the inserted node
    let append (node : XmlNode) (newNode : XmlNode) =
        Argument.validateNotNull node "node"
        Argument.validateNotNull newNode "newNode"
        let newNode = newNode |> importNode node.OwnerDocument
        node.ParentNode.InsertAfter(newNode, node) |> ignore
        newNode

    /// Inserts the specified XML nodes after the specified node, returns the inserted nodes
    let appendMany (node : XmlNode) (newNodes : seq<XmlNode>) =
        let insertOp newChild refChild (parentNode : XmlNode) = parentNode.InsertAfter(newChild, refChild)
        insertGeneric node newNodes insertOp

    /// Inserts the specified XML node before the specified node, returns the inserted node
    let prepend (node : XmlNode) (newNode : XmlNode) =
        Argument.validateNotNull node "node"
        Argument.validateNotNull newNode "newNode"
        let newNode = newNode |> importNode node.OwnerDocument
        node.ParentNode.InsertBefore(newNode, node) |> ignore
        newNode

    /// Inserts the specified XML nodes before the specified node, returns the inserted nodes
    let prependMany (node : XmlNode) (newNodes : seq<XmlNode>) =
        Argument.validateNotNull newNodes "newNodes"
        let insertOp newChild refChild (parentNode : XmlNode) = parentNode.InsertBefore(newChild, refChild)
        (insertGeneric node (newNodes |> List.ofSeq |> List.rev) insertOp) |> List.rev

    let private insertChildGeneric (node : XmlNode) (newNode : XmlNode) (insertOp : XmlNode -> XmlNode -> unit) =
        Argument.validateNotNull node "node"
        Argument.validateNotNull newNode "newNode"
        let newNode = newNode |> importNode node.OwnerDocument
        insertOp node newNode
        newNode

    let private insertChildrenGeneric (node : XmlNode) (newNodes : seq<XmlNode>) (insertOp : XmlNode -> XmlNode -> unit) =
        let rec insertRec (node : XmlNode) (newNodes : XmlNode list) appended =
            match newNodes with
            | [] -> appended
            | h :: t -> 
                let newNode = h |> importNode node.OwnerDocument
                insertOp node newNode
                insertRec node t (newNode :: appended)
        Argument.validateNotNull node "node"
        Argument.validateNotNull newNodes "newNodes"
        (insertRec node (List.ofSeq newNodes) []) |> List.rev

    /// Inserts the specified XML node as the last child of the specified node, returns the inserted node
    let appendChild (node : XmlNode) (newNode : XmlNode) =
        insertChildGeneric node newNode (fun parent newNode -> parent.AppendChild newNode |> ignore)

    /// Inserts the specified XML nodes as the last children of the specified node, returns the inserted nodes
    let appendChildren (node : XmlNode) (newNodes : seq<XmlNode>) =
        let insertOp (parentNode : XmlNode) (newNode : XmlNode) = parentNode.AppendChild newNode |> ignore
        insertChildrenGeneric node newNodes insertOp

    /// Inserts the specified XML node as the first child of the specified node, returns the inserted node
    let prependChild (node : XmlNode) (newNode : XmlNode) =
        insertChildGeneric node newNode (fun parent newNode -> parent.PrependChild newNode |> ignore)

    /// Inserts the specified XML nodes as the first children of the specified node, returns the inserted nodes
    let prependChildren (node : XmlNode) (newNodes : seq<XmlNode>) =
        Argument.validateNotNull newNodes "newNodes"
        let insertOp (parentNode : XmlNode) (newNode : XmlNode) = parentNode.PrependChild newNode |> ignore
        (insertChildrenGeneric node (newNodes |> List.ofSeq |> List.rev) insertOp) |> List.rev

    /// Replaces the specified XML node with a new node, returns the inserted node
    let replace (node : XmlNode) (newNode : XmlNode) =
        Argument.validateNotNull node "node"
        Argument.validateNotNull newNode "newNode"
        let parent = node.ParentNode
        let imported = importNode parent.OwnerDocument newNode
        parent.ReplaceChild(imported, node) |> ignore
        imported

    /// Replaces the specified XML node with a list of new nodes, returns the inserted nodes
    let replaceMany (node : XmlNode) (newNodes : seq<XmlNode>) =
        Argument.validateNotNull node "node"
        Argument.validateNotNull newNodes "newNodes"
        let inserted = appendMany node newNodes
        node.ParentNode.RemoveChild(node) |> ignore
        inserted

    /// Removes the specified XML node, returns the removed node
    let remove (node : XmlNode) =
        Argument.validateNotNull node "node"
        node.ParentNode.RemoveChild(node) |> ignore
        node

    /// Removes the specified XML nodes, returns the removed nodes
    let removeMany (nodes : seq<XmlNode>) =
        Argument.validateNotNull nodes "nodes"
        for node in nodes do node.ParentNode.RemoveChild(node) |> ignore
        List.ofSeq nodes

    /// Replaces the specified XML element in a document with the element's child elements, returns the moved elements
    let unwrap (element : XmlElement) = 
        let rec unwrapRec (parent : XmlNode) childNodes unwrapped = 
            match childNodes with
            | [] -> unwrapped
            | h :: t ->
                parent.InsertBefore(h, element) |> ignore
                unwrapRec parent t (h :: unwrapped)
        Argument.validateNotNull element "element"
        let parent = element.ParentNode
        let childNodes = element.ChildNodes |> Seq.cast<XmlNode> |> Seq.filter (fun n -> n :? XmlElement) |> List.ofSeq
        let unwrapped = unwrapRec parent childNodes []
        parent.RemoveChild(element) |> ignore
        unwrapped |> List.rev

    /// Replaces the specified nodes with a new node which contains the specified nodes, returns the moved nodes
    let wrap (elementName : string) (nodes : seq<XmlNode>) =
        let rec moveNodes nodes (parent : XmlNode) moved =
            match nodes with
            | [] -> moved
            | h :: t ->
                let imported = importNode parent.OwnerDocument h
                parent.AppendChild(imported) |> ignore
                moveNodes t parent (imported :: moved)
        Argument.validateNotNull elementName "elementName"
        Argument.validateNotNull nodes "nodes"
        let childNodes = nodes |> List.ofSeq
        if childNodes.Length = 0 then [] else
            let firstChild = childNodes |> List.head
            let parent = firstChild.OwnerDocument.CreateElement(elementName)
            prependMany firstChild (Seq.singleton (parent :> XmlNode)) |> ignore
            (moveNodes childNodes parent []) |> List.rev