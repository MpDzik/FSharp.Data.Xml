namespace FSharp.Data.Xml.Tests

open System
open System.IO
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