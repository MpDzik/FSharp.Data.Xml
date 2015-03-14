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