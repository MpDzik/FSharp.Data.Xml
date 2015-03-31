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
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let root = Xml.root document
        Assert.Equal(XmlNodeType.Element, root.NodeType)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><baz>c</baz></root>", root.OuterXml)

    [<Fact>]
    let ``tryRoot should return None when document is null``() =
        let result = Xml.tryRoot null
        Assert.Equal(Option<XmlElement>.None, result)

    [<Fact>]
    let ``tryRoot should return root node of document``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
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
        let document = xmlDecl + "<root><foo>a</foo><foo>b</foo><foo>c</foo></root>" |> Xml.ofString
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
        let document = xmlDecl + xml |> Xml.ofString
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
        let document = xmlDecl + xml |> Xml.ofString
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
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let result = document |> Xml.querySingle "/root/bar"
        Assert.Equal("<bar>b</bar>", result.OuterXml)

    [<Fact>]
    let ``querySingle should perform XPath query and return first result node``() =
        let document = xmlDecl + "<root><foo>a</foo><foo>b</foo><foo>c</foo></root>" |> Xml.ofString
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
        let document = xmlDecl + xml |> Xml.ofString
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
        let document = xmlDecl + xml |> Xml.ofString
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
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let result = document |> Xml.tryQuerySingle "/root/bar"
        Assert.Equal("<bar>b</bar>", result.Value.OuterXml)

    [<Fact>]
    let ``tryQuerySingle should perform XPath query and return first result node``() =
        let document = xmlDecl + "<root><foo>a</foo><foo>b</foo><foo>c</foo></root>" |> Xml.ofString
        let result = document |> Xml.tryQuerySingle "/root/foo"
        Assert.Equal("<foo>a</foo>", result.Value.OuterXml)

    [<Fact>]
    let ``tryQuerySingle should return None if no results were returned``() =
        let document = xmlDecl + "<root><foo>a</foo><foo>b</foo><foo>c</foo></root>" |> Xml.ofString
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
        let document = xmlDecl + xml |> Xml.ofString
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
        let document = xmlDecl + xml |> Xml.ofString
        let nsmgr = Xml.nsmgr document
        nsmgr.AddNamespace("x", "http://test1")
        nsmgr.AddNamespace("y", "http://test2")
        let result = document |> Xml.tryQuerySingle "/root/y:foo"
        Assert.Equal("<y:foo xmlns:y=\"http://test2\">b</y:foo>", result.Value.OuterXml)

    [<Fact>]
    let ``append should throw exception when node is null``() =
        let newNode = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.append null newNode |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``append should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.append node null |> ignore)
        Assert.Equal("newNode", ex.ParamName)

    [<Fact>]
    let ``append should insert sibbling node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.append node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><quux>q</quux><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``append should insert sibbling node after first node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/foo"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.append node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><foo>a</foo><quux>q</quux><bar>b</bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``append should insert sibbling node after last node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/baz"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.append node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><baz>c</baz><quux>q</quux></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendMany should throw exception when node is null``() =
        let newNodes = [| xmlDecl + "<root />" |> Xml.ofString |> Xml.root |]
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.appendMany null newNodes |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``appendMany should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.appendMany node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``appendMany should insert sibbling nodes``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.appendMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><quux>q1</quux><quux>q2</quux><quux>q3</quux><baz>c</baz></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendMany should insert sibbling node after first node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/foo"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.appendMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><quux>q1</quux><quux>q2</quux><quux>q3</quux><bar>b</bar><baz>c</baz></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendMany should insert sibbling node after last node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/baz"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.appendMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><baz>c</baz><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prepend should throw exception when node is null``() =
        let newNode = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prepend null newNode |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``prepend should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prepend node null |> ignore)
        Assert.Equal("newNode", ex.ParamName)

    [<Fact>]
    let ``prepend should insert sibbling node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prepend node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><foo>a</foo><quux>q</quux><bar>b</bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prepend should insert sibbling node before first node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/foo"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prepend node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><quux>q</quux><foo>a</foo><bar>b</bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prepend should insert sibbling node before last node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/baz"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prepend node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><quux>q</quux><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependMany should throw exception when node is null``() =
        let newNodes = [| xmlDecl + "<root />" |> Xml.ofString |> Xml.root |]
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prependMany null newNodes |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``prependMany should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prependMany node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``prependMany should insert sibbling node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.prependMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><quux>q1</quux><quux>q2</quux><quux>q3</quux><bar>b</bar><baz>c</baz></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependMany should insert sibbling node before first node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/foo"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.prependMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><quux>q1</quux><quux>q2</quux><quux>q3</quux><foo>a</foo><bar>b</bar><baz>c</baz></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependMany should insert sibbling node before last node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar>b</bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/baz"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.prependMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar><quux>q1</quux><quux>q2</quux><quux>q3</quux><baz>c</baz></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendChild should throw exception when node is null``() =
        let newNode = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.appendChild null newNode |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``appendChild should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.appendChild node null |> ignore)
        Assert.Equal("newNode", ex.ParamName)

    [<Fact>]
    let ``appendChild should append child node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.appendChild node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><child><foo>a</foo><bar>b</bar><baz>c</baz><quux>q</quux></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendChild should insert child node into empty node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar></bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.appendChild node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><foo>a</foo><bar><quux>q</quux></bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendChildren should throw exception when node is null``() =
        let newNodes = [| xmlDecl + "<root />" |> Xml.ofString |> Xml.root |]
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.appendChildren null newNodes |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``appendChildren should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.appendChildren node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``appendChildren should append child node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.appendChildren node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><child><foo>a</foo><bar>b</bar><baz>c</baz><quux>q</quux></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendChildren should append multiple child nodes``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.appendChildren node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><child><foo>a</foo><bar>b</bar><baz>c</baz><quux>q1</quux><quux>q2</quux><quux>q3</quux></child></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``appendChildren should insert child node into empty node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar></bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.appendChildren node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><bar><quux>q</quux></bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependChild should throw exception when node is null``() =
        let newNode = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prependChild null newNode |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``prependChild should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prependChild node null |> ignore)
        Assert.Equal("newNode", ex.ParamName)

    [<Fact>]
    let ``prependChild should prepend child node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prependChild node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><child><quux>q</quux><foo>a</foo><bar>b</bar><baz>c</baz></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependChild should insert child node into empty node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar></bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prependChild node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><foo>a</foo><bar><quux>q</quux></bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependChildren should throw exception when node is null``() =
        let newNodes = [| xmlDecl + "<root />" |> Xml.ofString |> Xml.root |]
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prependChildren null newNodes |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``prependChildren should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.prependChildren node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``prependChildren should prepend child node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prependChildren node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><child><quux>q</quux><foo>a</foo><bar>b</bar><baz>c</baz></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependChildren should prepend multiple child nodes``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.prependChildren node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><child><quux>q1</quux><quux>q2</quux><quux>q3</quux><foo>a</foo><bar>b</bar><baz>c</baz></child></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``prependChildren should insert child node into empty node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar></bar><baz>c</baz></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.prependChildren node [| newNode |]
        Assert.Equal(1, result.Length)
        Assert.Equal("<quux>q</quux>", (result |> List.head).OuterXml)
        Assert.Equal("<root><foo>a</foo><bar><quux>q</quux></bar><baz>c</baz></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``replace should throw exception when node is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.replace null node |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``replace should throw exception when newNode is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.replace node null |> ignore)
        Assert.Equal("newNode", ex.ParamName)

    [<Fact>]
    let ``replace should replace first node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child/foo"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.replace node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><child><quux>q</quux><bar>b</bar><baz>c</baz></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``replace should replace middle node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child/bar"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.replace node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><child><foo>a</foo><quux>q</quux><baz>c</baz></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``replace should replace last node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child/baz"
        let newNode = xmlDecl + "<quux>q</quux>" |> Xml.ofString |> Xml.root
        let result = Xml.replace node newNode
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><child><foo>a</foo><bar>b</bar><quux>q</quux></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``replaceMany should throw exception when node is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.replaceMany null [] |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Fact>]
    let ``replaceMany should throw exception when newNodes is null``() =
        let node = xmlDecl + "<root />" |> Xml.ofString |> Xml.root
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.replaceMany node null |> ignore)
        Assert.Equal("newNodes", ex.ParamName)

    [<Fact>]
    let ``replaceMany should replace first node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child/foo"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.replaceMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><child><quux>q1</quux><quux>q2</quux><quux>q3</quux><bar>b</bar><baz>c</baz></child></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``replaceMany should replace middle node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child/bar"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.replaceMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><child><foo>a</foo><quux>q1</quux><quux>q2</quux><quux>q3</quux><baz>c</baz></child></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``replaceMany should replace last node``() =
        let document = xmlDecl + "<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>" |> Xml.ofString
        let node = document |> Xml.querySingle "/root/child/baz"
        let newNodes = xmlDecl + "<root><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>" |> Xml.ofString |> Xml.query "/root/quux"
        let result = Xml.replaceMany node newNodes
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><child><foo>a</foo><bar>b</bar><quux>q1</quux><quux>q2</quux><quux>q3</quux></child></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``remove should throw exception when node is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.remove null |> ignore)
        Assert.Equal("node", ex.ParamName)

    [<Theory>]
    [<InlineData("<root><child><foo>a</foo><bar>b</bar><quux>q</quux><baz>c</baz></child></root>")>]
    [<InlineData("<root><child><quux>q</quux><foo>a</foo><bar>b</bar><baz>c</baz></child></root>")>]
    [<InlineData("<root><child><foo>a</foo><bar>b</bar><baz>c</baz><quux>q</quux></child></root>")>]
    let ``remove should remove node``(xml : string) =
        let document = xmlDecl + xml |> Xml.ofString
        let result = Xml.remove (document |> Xml.querySingle "/root/child/quux")
        Assert.Equal("<quux>q</quux>", result.OuterXml)
        Assert.Equal("<root><child><foo>a</foo><bar>b</bar><baz>c</baz></child></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``removeMany should throw exception when nodes is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.removeMany null |> ignore)
        Assert.Equal("nodes", ex.ParamName)

    [<Theory>]
    [<InlineData("<root><foo>a</foo><quux>q1</quux><quux>q2</quux><quux>q3</quux><bar>b</bar></root>")>]
    [<InlineData("<root><quux>q1</quux><quux>q2</quux><quux>q3</quux><foo>a</foo><bar>b</bar></root>")>]
    [<InlineData("<root><foo>a</foo><bar>b</bar><quux>q1</quux><quux>q2</quux><quux>q3</quux></root>")>]
    [<InlineData("<root><quux>q1</quux><foo>a</foo><quux>q2</quux><bar>b</bar><quux>q3</quux></root>")>]
    let ``removeMany should remove nodes``(xml : string) =
        let document = xmlDecl + xml |> Xml.ofString
        let result = Xml.removeMany (document |> Xml.query "/root/quux")
        Assert.Equal(3, result.Length)
        Assert.Equal("<quux>q1</quux>", result.[0].OuterXml)
        Assert.Equal("<quux>q2</quux>", result.[1].OuterXml)
        Assert.Equal("<quux>q3</quux>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><bar>b</bar></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``unwrap should throw exception when element is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.unwrap null |> ignore)
        Assert.Equal("element", ex.ParamName)

    [<Fact>]
    let ``unwrap should move single child element to parent``() =
        let document = xmlDecl + "<root><foo>a</foo><bar><baz>c</baz></bar><quux>d</quux></root>" |> Xml.ofString
        let result = Xml.unwrap (document |> (Xml.querySingle "/root/bar") :?> XmlElement)
        Assert.Equal(1, result.Length)
        Assert.Equal("<baz>c</baz>", result.[0].OuterXml)
        Assert.Equal("<root><foo>a</foo><baz>c</baz><quux>d</quux></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``unwrap should move multiple child elements to parent``() =
        let document = xmlDecl + "<root><foo>a</foo><bar><baz>c1</baz><baz>c2</baz><baz>c3</baz></bar><quux>d</quux></root>"
                       |> Xml.ofString
        let result = Xml.unwrap (document |> (Xml.querySingle "/root/bar") :?> XmlElement)
        Assert.Equal(3, result.Length)
        Assert.Equal("<baz>c1</baz>", result.[0].OuterXml)
        Assert.Equal("<baz>c2</baz>", result.[1].OuterXml)
        Assert.Equal("<baz>c3</baz>", result.[2].OuterXml)
        Assert.Equal("<root><foo>a</foo><baz>c1</baz><baz>c2</baz><baz>c3</baz><quux>d</quux></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``unwrap should handle empty node``() =
        let document = xmlDecl + "<root><foo>a</foo><bar></bar><quux>d</quux></root>" |> Xml.ofString
        let result = Xml.unwrap (document |> (Xml.querySingle "/root/bar") :?> XmlElement)
        Assert.Equal(0, result.Length)
        Assert.Equal("<root><foo>a</foo><quux>d</quux></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``wrap should throw exception when elementName is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.wrap null [] |> ignore)
        Assert.Equal("elementName", ex.ParamName)

    [<Fact>]
    let ``wrap should throw exception when nodes is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.wrap "foo" null |> ignore)
        Assert.Equal("nodes", ex.ParamName)

    [<Fact>]
    let ``wrap should handle single element``() =
        let document = xmlDecl + "<root><foo>a</foo><baz>c</baz><quux>d</quux></root>" |> Xml.ofString
        let result = Xml.query "/root/baz" document |> Xml.wrap "bar"
        Assert.Equal("<bar><baz>c</baz></bar>", result.Value.OuterXml)
        Assert.Equal("<root><foo>a</foo><bar><baz>c</baz></bar><quux>d</quux></root>", (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``wrap should handle multiple elements``() =
        let document = xmlDecl + "<root><foo>a</foo><baz>c1</baz><baz>c2</baz><baz>c3</baz><quux>d</quux></root>"
                       |> Xml.ofString
        let result = Xml.query "/root/baz" document |> Xml.wrap "bar"
        Assert.Equal("<bar><baz>c1</baz><baz>c2</baz><baz>c3</baz></bar>", result.Value.OuterXml)
        Assert.Equal("<root><foo>a</foo><bar><baz>c1</baz><baz>c2</baz><baz>c3</baz></bar><quux>d</quux></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``wrapNs should throw exception when elementName is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.wrapNs null "http://dummy" [] |> ignore)
        Assert.Equal("elementName", ex.ParamName)

    [<Fact>]
    let ``wrapNs should throw exception when namespace is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.wrapNs "x:foo" null [] |> ignore)
        Assert.Equal("namespaceUri", ex.ParamName)

    [<Fact>]
    let ``wrapNs should throw exception when nodes is null``() =
        let ex = Assert.Throws<ArgumentNullException>(fun () -> Xml.wrapNs "x:foo" "http://dummy" null |> ignore)
        Assert.Equal("nodes", ex.ParamName)

    [<Fact>]
    let ``wrapNs should handle single element``() =
        let document = xmlDecl + "<root><foo>a</foo><baz>c</baz><quux>d</quux></root>" |> Xml.ofString
        let result = Xml.query "/root/baz" document |> Xml.wrapNs "x:bar" "http://dummy"
        Assert.Equal("<x:bar xmlns:x=\"http://dummy\"><baz>c</baz></x:bar>", result.Value.OuterXml)
        Assert.Equal("<root><foo>a</foo><x:bar xmlns:x=\"http://dummy\"><baz>c</baz></x:bar><quux>d</quux></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``wrapNs should handle multiple elements``() =
        let document = xmlDecl + "<root><foo>a</foo><baz>c1</baz><baz>c2</baz><baz>c3</baz><quux>d</quux></root>"
                       |> Xml.ofString
        let result = Xml.query "/root/baz" document |> Xml.wrapNs "x:bar" "http://dummy"
        Assert.Equal("<x:bar xmlns:x=\"http://dummy\"><baz>c1</baz><baz>c2</baz><baz>c3</baz></x:bar>", result.Value.OuterXml)
        Assert.Equal("<root><foo>a</foo><x:bar xmlns:x=\"http://dummy\"><baz>c1</baz><baz>c2</baz><baz>c3</baz></x:bar><quux>d</quux></root>",
                     (document |> Xml.root).OuterXml)

    [<Fact>]
    let ``createElem should throw exception when name is null``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createElem null "foo" |> ignore)
        Assert.Equal("name", ex.ParamName)

    [<Fact>]
    let ``createElem should throw exception when name is empty``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createElem "" "foo" |> ignore)
        Assert.Equal("name", ex.ParamName)

    [<Fact>]
    let ``createElem should create element with inner text``() =
        let elem = Xml.createElem "foo" "foo-value"
        Assert.Equal("foo", elem.Name)
        Assert.Equal("foo-value", elem.InnerXml)

    [<Fact>]
    let ``createElem should create element with inner xml``() =
        let elem = Xml.createElem "foo" "<bar>value</bar>"
        Assert.Equal("foo", elem.Name)
        Assert.Equal("<bar>value</bar>", elem.InnerXml)

    [<Fact>]
    let ``createElem should create empty element``() =
        let elem = Xml.createElem "foo" null
        Assert.Equal("foo", elem.Name)
        Assert.Empty(elem.InnerXml)

    [<Fact>]
    let ``createTextElem should throw exception when name is null``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createTextElem null "foo" |> ignore)
        Assert.Equal("name", ex.ParamName)

    [<Fact>]
    let ``createTextElem should throw exception when name is empty``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createTextElem "" "foo" |> ignore)
        Assert.Equal("name", ex.ParamName)

    [<Fact>]
    let ``createTextElem should create element with inner text``() =
        let elem = Xml.createTextElem "foo" "foo-value"
        Assert.Equal("foo", elem.Name)
        Assert.Equal("foo-value", elem.InnerText)

    [<Fact>]
    let ``createTextElem should escape inner XML``() =
        let elem = Xml.createTextElem "foo" "<bar>value</bar>"
        Assert.Equal("foo", elem.Name)
        Assert.Equal("<bar>value</bar>", elem.InnerText)
        Assert.Equal("&lt;bar&gt;value&lt;/bar&gt;", elem.InnerXml)

    [<Fact>]
    let ``createElem should create empty element``() =
        let elem = Xml.createElem "foo" null
        Assert.Equal("foo", elem.Name)
        Assert.Empty(elem.InnerXml)

    [<Fact>]
    let ``createElemNs should throw exception when name is null``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createElemNs null "http://dummy" "foo" |> ignore)
        Assert.Equal("qualifiedName", ex.ParamName)

    [<Fact>]
    let ``createElemNs should throw exception when name is empty``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createElemNs "" "http://dummy" "foo" |> ignore)
        Assert.Equal("qualifiedName", ex.ParamName)

    [<Fact>]
    let ``createElemNs should throw exception when namespace is null``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createElemNs "foo" null "foo" |> ignore)
        Assert.Equal("namespaceUri", ex.ParamName)

    [<Fact>]
    let ``createElemNs should throw exception when namespace is empty``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createElemNs "foo" "" "foo" |> ignore)
        Assert.Equal("namespaceUri", ex.ParamName)

    [<Fact>]
    let ``createElemNs should create element with inner text``() =
        let elem = Xml.createElemNs "x:foo" "http://dummy" "foo-value"
        Assert.Equal("x:foo", elem.Name)
        Assert.Equal("http://dummy", elem.NamespaceURI)
        Assert.Equal("foo-value", elem.InnerXml)

    [<Fact>]
    let ``createElemNs should create element with inner xml``() =
        let elem = Xml.createElemNs "x:foo" "http://dummy" "<bar>value</bar>"
        Assert.Equal("x:foo", elem.Name)
        Assert.Equal("http://dummy", elem.NamespaceURI)
        Assert.Equal("<bar>value</bar>", elem.InnerXml)

    [<Fact>]
    let ``createElemNs should create empty element``() =
        let elem = Xml.createElemNs "foo" "http://dummy" null
        Assert.Equal("foo", elem.Name)
        Assert.Equal("http://dummy", elem.NamespaceURI)
        Assert.Empty(elem.InnerXml)

    [<Fact>]
    let ``createTextElemNs should throw exception when name is null``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createTextElemNs null "http://dummy" "foo" |> ignore)
        Assert.Equal("qualifiedName", ex.ParamName)

    [<Fact>]
    let ``createTextElemNs should throw exception when name is empty``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createTextElemNs "" "http://dummy" "foo" |> ignore)
        Assert.Equal("qualifiedName", ex.ParamName)

    [<Fact>]
    let ``createTextElemNs should throw exception when namespace is null``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createTextElemNs "foo" null "foo" |> ignore)
        Assert.Equal("namespaceUri", ex.ParamName)

    [<Fact>]
    let ``createTextElemNs should throw exception when namespace is empty``() =
        let ex = Assert.Throws<ArgumentException>(fun () -> Xml.createTextElemNs "foo" "" "foo" |> ignore)
        Assert.Equal("namespaceUri", ex.ParamName)

    [<Fact>]
    let ``createTextElemNs should create element with inner text``() =
        let elem = Xml.createTextElemNs "x:foo" "http://dummy" "foo-value"
        Assert.Equal("x:foo", elem.Name)
        Assert.Equal("http://dummy", elem.NamespaceURI)
        Assert.Equal("foo-value", elem.InnerText)

    [<Fact>]
    let ``createTextElemNs should escape inner XML``() =
        let elem = Xml.createTextElemNs "x:foo" "http://dummy" "<bar>value</bar>"
        Assert.Equal("x:foo", elem.Name)
        Assert.Equal("http://dummy", elem.NamespaceURI)
        Assert.Equal("<bar>value</bar>", elem.InnerText)
        Assert.Equal("&lt;bar&gt;value&lt;/bar&gt;", elem.InnerXml)

    [<Fact>]
    let ``createTextElemNs should create empty element``() =
        let elem = Xml.createTextElemNs "foo" "http://dummy" null
        Assert.Equal("foo", elem.Name)
        Assert.Equal("http://dummy", elem.NamespaceURI)
        Assert.Empty(elem.InnerText)
