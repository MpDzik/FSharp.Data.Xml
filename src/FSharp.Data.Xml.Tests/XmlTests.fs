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