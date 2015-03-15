namespace FSharp.Data.Xml.Tests

open System
open System.IO
open System.Xml
open FSharp.Data.Xml
open Xunit

type XmlTests() =

    let xmlDecl = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"

    [<Fact>]
    let ``ofStream should throw exception when stream is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.ofStream null |> ignore)
        Assert.Equal("stream", ex.ParamName)
        
    [<Fact>]
    let ``ofStream should load XML document from stream``() =
        let xml = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>"
        use stream = new MemoryStream()
        let writer = new StreamWriter(stream)
        writer.WriteLine(xml)
        writer.Flush()
        stream.Flush()
        stream.Position <- 0L
        let document = Xml.ofStream stream
        Assert.Equal(xml, document.OuterXml)

    [<Fact>]
    let ``ofString should throw exception when string is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.ofString null |> ignore)
        Assert.Equal("xml", ex.ParamName)

    [<Fact>]
    let ``ofString should load XML document from string``() =
        let xml = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>"
        let document = Xml.ofString xml
        Assert.Equal(xml, document.OuterXml)

    [<Fact>]
    let ``nsmgr should throw exception when document is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.nsmgr null |> ignore)
        Assert.Equal("document", ex.ParamName)

    [<Fact>]
    let ``nsmgr should return namespace manager for document``() =
        let document = XmlDocument()
        let nsmgr = Xml.nsmgr document
        Assert.Equal(document.NameTable, nsmgr.NameTable)

    [<Fact>]
    let ``nsmgr should cache created namespcae managers``() =
        let document = XmlDocument()
        let nsmgr1 = Xml.nsmgr document
        let nsmgr2 = Xml.nsmgr document
        Assert.Same(nsmgr1, nsmgr2)

    [<Fact>]
    let ``root should thrown exception when document is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.root null |> ignore)
        Assert.Equal("document", ex.ParamName)

    [<Fact>]
    let ``root should return root node of document``() =
        let xml = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>"
        let document = Xml.ofString xml
        let root = Xml.root document
        Assert.Equal(XmlNodeType.Element, root.NodeType)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><baz>c</baz></root>", root.OuterXml)

    [<Fact>]
    let ``tryRoot should return None when document is null``() =
        let result = Xml.tryRoot null
        Assert.Equal(Option<XmlElement>.None, result)

    [<Fact>]
    let ``tryRoot should return root node of document``() =
        let xml = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>"
        let document = Xml.ofString xml
        let root = Xml.tryRoot document
        Assert.Equal(XmlNodeType.Element, root.Value.NodeType)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><baz>c</baz></root>", root.Value.OuterXml)

    [<Fact>]
    let ``tryRoot should return None when document does not have a root node``() =
        let document = XmlDocument()
        let result = Xml.tryRoot document
        Assert.Equal(Option<XmlElement>.None, result)

    [<Fact>]
    let ``query should throw exception when XPath null``() =
        let document = XmlDocument()
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.query null document |> ignore)
        Assert.Equal("xpath", ex.ParamName)

    [<Fact>]
    let ``query should throw exception when document null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.query "\\root" null |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``query should perform XPath query and return results``() =
        let xml = xmlDecl + "<root><foo>a</foo><foo>b</foo><foo>c</foo></root>"
        let document = Xml.ofString xml
        let result = document |> Xml.query "/root/foo" |> Array.ofSeq
        Assert.Equal(3, result.Length)
        Assert.Equal("<foo>a</foo>", result.[0].OuterXml)
        Assert.Equal("<foo>b</foo>", result.[1].OuterXml)
        Assert.Equal("<foo>c</foo>", result.[2].OuterXml)

    [<Fact>]
    let ``query should perform XPath query with XML namespaces``() =
        let xml = 
            @"<root xmlns:x='http://test1' xmlns:y='http://test2'>
                <x:foo>a</x:foo>
                <y:foo>b</y:foo>
                <x:foo>c</x:foo>
             </root>"
        let document = Xml.ofString (xmlDecl + xml)
        let nsmgr = Xml.nsmgr document
        nsmgr.AddNamespace("x", "http://test1")
        nsmgr.AddNamespace("y", "http://test2")
        let root = Xml.root document
        let result = root |> Xml.query "/root/x:foo" |> Array.ofSeq
        Assert.Equal(2, result.Length)
        Assert.Equal("<x:foo xmlns:x=\"http://test1\">a</x:foo>", result.[0].OuterXml)
        Assert.Equal("<x:foo xmlns:x=\"http://test1\">c</x:foo>", result.[1].OuterXml)

    [<Fact>]
    let ``query should perform XPath query with XML namespaces on document``() =
        let xml = 
            @"<root xmlns:x='http://test1' xmlns:y='http://test2'>
                <x:foo>a</x:foo>
                <y:foo>b</y:foo>
                <x:foo>c</x:foo>
             </root>"
        let document = Xml.ofString (xmlDecl + xml)
        let nsmgr = Xml.nsmgr document
        nsmgr.AddNamespace("x", "http://test1")
        nsmgr.AddNamespace("y", "http://test2")
        let result = document |> Xml.query "/root/x:foo" |> Array.ofSeq
        Assert.Equal(2, result.Length)
        Assert.Equal("<x:foo xmlns:x=\"http://test1\">a</x:foo>", result.[0].OuterXml)
        Assert.Equal("<x:foo xmlns:x=\"http://test1\">c</x:foo>", result.[1].OuterXml)

    [<Fact>]
    let ``querySingle should throw exception when XPath null``() =
        let document = XmlDocument()
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.querySingle null document |> ignore)
        Assert.Equal("xpath", ex.ParamName)

    [<Fact>]
    let ``querySingle should throw exception when document null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.querySingle "\\root" null |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``querySingle should perform XPath query and return result``() =
        let xml = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>"
        let document = Xml.ofString xml
        let result = document |> Xml.querySingle "/root/bar"
        Assert.Equal("<bar>b</bar>", result.OuterXml)

    [<Fact>]
    let ``querySingle should perform XPath query and return first result node``() =
        let xml = xmlDecl + "<root><foo>a</foo><foo>b</foo><foo>c</foo></root>"
        let document = Xml.ofString xml
        let result = document |> Xml.querySingle "/root/foo"
        Assert.Equal("<foo>a</foo>", result.OuterXml)
        
    [<Fact>]
    let ``querySingle should perform XPath query with XML namespaces``() =
        let xml = 
            @"<root xmlns:x='http://test1' xmlns:y='http://test2'>
                <x:foo>a</x:foo>
                <y:foo>b</y:foo>
                <x:foo>c</x:foo>
             </root>"
        let document = Xml.ofString (xmlDecl + xml)
        let nsmgr = Xml.nsmgr document
        nsmgr.AddNamespace("x", "http://test1")
        nsmgr.AddNamespace("y", "http://test2")
        let root = Xml.root document
        let result = root |> Xml.querySingle "/root/y:foo"
        Assert.Equal("<y:foo xmlns:y=\"http://test2\">b</y:foo>", result.OuterXml)

    [<Fact>]
    let ``querySingle should perform XPath query with XML namespaces on document``() =
        let xml = 
            @"<root xmlns:x='http://test1' xmlns:y='http://test2'>
                <x:foo>a</x:foo>
                <y:foo>b</y:foo>
                <x:foo>c</x:foo>
             </root>"
        let document = Xml.ofString (xmlDecl + xml)
        let nsmgr = Xml.nsmgr document
        nsmgr.AddNamespace("x", "http://test1")
        nsmgr.AddNamespace("y", "http://test2")
        let result = document |> Xml.querySingle "/root/y:foo"
        Assert.Equal("<y:foo xmlns:y=\"http://test2\">b</y:foo>", result.OuterXml)

    [<Fact>]
    let ``tryQuerySingle should return None when XPath null``() =
        let document = XmlDocument()
        let result = Xml.tryQuerySingle null document
        Assert.Equal(Option<XmlNode>.None, result)

    [<Fact>]
    let ``tryQuerySingle should return None when document null``() =
        let result = Xml.tryQuerySingle "\\root" null
        Assert.Equal(Option<XmlNode>.None, result)

    [<Fact>]
    let ``tryQuerySingle should perform XPath query and return result``() =
        let xml = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>"
        let document = Xml.ofString xml
        let result = document |> Xml.tryQuerySingle "/root/bar"
        Assert.Equal("<bar>b</bar>", result.Value.OuterXml)

    [<Fact>]
    let ``tryQuerySingle should perform XPath query and return first result node``() =
        let xml = xmlDecl + "<root><foo>a</foo><foo>b</foo><foo>c</foo></root>"
        let document = Xml.ofString xml
        let result = document |> Xml.tryQuerySingle "/root/foo"
        Assert.Equal("<foo>a</foo>", result.Value.OuterXml)

    [<Fact>]
    let ``tryQuerySingle should return None if no results were returned``() =
        let xml = xmlDecl + "<root><foo>a</foo><foo>b</foo><foo>c</foo></root>"
        let document = Xml.ofString xml
        let result = document |> Xml.tryQuerySingle "/root/quux"
        Assert.Equal(Option<XmlNode>.None, result)
        
    [<Fact>]
    let ``tryQuerySingle should perform XPath query with XML namespaces``() =
        let xml = 
            @"<root xmlns:x='http://test1' xmlns:y='http://test2'>
                <x:foo>a</x:foo>
                <y:foo>b</y:foo>
                <x:foo>c</x:foo>
             </root>"
        let document = Xml.ofString (xmlDecl + xml)
        let nsmgr = Xml.nsmgr document
        nsmgr.AddNamespace("x", "http://test1")
        nsmgr.AddNamespace("y", "http://test2")
        let root = Xml.root document
        let result = root |> Xml.tryQuerySingle "/root/y:foo"
        Assert.Equal("<y:foo xmlns:y=\"http://test2\">b</y:foo>", result.Value.OuterXml)

    [<Fact>]
    let ``tryQuerySingle should perform XPath query with XML namespaces on document``() =
        let xml = 
            @"<root xmlns:x='http://test1' xmlns:y='http://test2'>
                <x:foo>a</x:foo>
                <y:foo>b</y:foo>
                <x:foo>c</x:foo>
             </root>"
        let document = Xml.ofString (xmlDecl + xml)
        let nsmgr = Xml.nsmgr document
        nsmgr.AddNamespace("x", "http://test1")
        nsmgr.AddNamespace("y", "http://test2")
        let result = document |> Xml.tryQuerySingle "/root/y:foo"
        Assert.Equal("<y:foo xmlns:y=\"http://test2\">b</y:foo>", result.Value.OuterXml)

    [<Fact>]
    let ``append should throw exception when node is null``() =
        let xml = xmlDecl + "<root />"
        let newNodes = [| Xml.ofString xml |> Xml.root :> XmlNode |]
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.append null newNodes |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``append should throw exception when newNodes is null``() =
        let xml = xmlDecl + "<root />"
        let node = Xml.ofString xml |> Xml.root :> XmlNode
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.append node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``append should insert sibbling node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.append node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><quux>q</quux><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``append should insert sibbling node after first node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/foo"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.append node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><quux>q</quux><bar>b</bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``append should insert sibbling node after last node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/baz"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.append node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><baz>c</baz><quux>q</quux></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``append should insert multiple sibbling nodes``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.append node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><quux>q1</quux><quux>q2</quux><quux>q3</quux><baz>c</baz></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prepend should throw exception when node is null``() =
        let xml = xmlDecl + "<root />"
        let newNodes = [| Xml.ofString xml |> Xml.root :> XmlNode |]
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prepend null newNodes |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``prepend should throw exception when newNodes is null``() =
        let xml = xmlDecl + "<root />"
        let node = Xml.ofString xml |> Xml.root :> XmlNode
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prepend node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``prepend should insert sibbling node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prepend node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><quux>q</quux><bar>b</bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prepend should insert sibbling node before first node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/foo"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prepend node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><quux>q</quux><foo>a</foo><bar>b</bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prepend should insert sibbling node before last node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/baz"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prepend node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><quux>q</quux><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``preppend should insert multiple sibbling nodes``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.prepend node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><quux>q1</quux><quux>q2</quux><quux>q3</quux><bar>b</bar><baz>c</baz></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendChild should throw exception when node is null``() =
        let xml = xmlDecl + "<root />"
        let newNodes = [| Xml.ofString xml |> Xml.root :> XmlNode |]
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.appendChild null newNodes |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``appendChild should throw exception when newNodes is null``() =
        let xml = xmlDecl + "<root />"
        let node = Xml.ofString xml |> Xml.root :> XmlNode
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.appendChild node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``appendChild should append child node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.appendChild node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><child><foo>a</foo><bar>b</bar><baz>c</baz><quux>q</quux></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendChild should append multiple child nodes``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.appendChild node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><child><foo>a</foo><bar>b</bar><baz>c</baz><quux>q1</quux><quux>q2</quux><quux>q3</quux></child></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendChild should insert child node into empty node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar></bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.appendChild node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><bar><quux>q</quux></bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependChild should throw exception when node is null``() =
        let xml = xmlDecl + "<root />"
        let newNodes = [| Xml.ofString xml |> Xml.root :> XmlNode |]
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prependChild null newNodes |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``prependChild should throw exception when newNodes is null``() =
        let xml = xmlDecl + "<root />"
        let node = Xml.ofString xml |> Xml.root :> XmlNode
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prependChild node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``prependChild should prepend child node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prependChild node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><child><quux>q</quux><foo>a</foo><bar>b</bar><baz>c</baz></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependChild should prepend multiple child nodes``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.prependChild node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><child><quux>q1</quux><quux>q2</quux><quux>q3</quux><foo>a</foo><bar>b</bar><baz>c</baz></child></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependChild should insert child node into empty node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar></bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prependChild node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><bar><quux>q</quux></bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)