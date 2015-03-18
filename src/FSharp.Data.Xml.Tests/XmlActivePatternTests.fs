namespace FSharp.Data.Xml.Tests

open System.Xml
open FSharp.Data.Xml
open Xunit

type XmlActivePatternTests() =

    let xml = @"<?xml version='1.0' encoding='utf-8'?>
                <root>
                    <foo bar='bar-value' baz='baz-value' />
                </root>"

    [<Fact>]
    let ``Match document``() =
        let document = Xml.ofString xml
        let element = document |> Xml.querySingle "/root"
        Assert.Equal(document, match document with XmlDocument d -> d | _ -> null)
        Assert.Null(match element with XmlDocument d -> d | _ -> null)

    let testNodes =
        let rec getNodesRec (node : XmlNode) =
            seq { yield node
                  for attr in node.Attributes do yield attr
                  for child in node.ChildNodes do yield! (getNodesRec child) }
        getNodesRec (Xml.ofString xml |> Xml.root)

    [<Fact>]
    let ``Match element``() =
        let result : string seq = 
            seq { for node in testNodes do match node with XmlElement e -> yield Some e | _ -> yield None }
                |> Seq.filter (fun r -> r.IsSome)
                |> Seq.map (fun r -> r.Value.Name)
        Assert.Equal([| "root"; "foo" |], result)

    [<Fact>]
    let ``Match element by name``() =
        let result : string seq = 
            seq { for node in testNodes do match node with XmlElem("foo") e -> yield Some e | _ -> yield None }
                |> Seq.filter (fun r -> r.IsSome)
                |> Seq.map (fun r -> r.Value.Name)
        Assert.Equal([| "foo" |], result)

    [<Fact>]
    let ``Match attribute``() =
        let result : string seq = 
            seq { for node in testNodes do match node with XmlAttribute a -> yield Some a | _ -> yield None }
                |> Seq.filter (fun r -> r.IsSome)
                |> Seq.map (fun r -> r.Value.Name)
        Assert.Equal([| "bar"; "baz" |], result)

    [<Fact>]
    let ``Match attribute by name``() =
        let result : string seq = 
            seq { for node in testNodes do match node with XmlAttr("baz") e -> yield Some e | _ -> yield None }
                |> Seq.filter (fun r -> r.IsSome)
                |> Seq.map (fun r -> r.Value.Name)
        Assert.Equal([| "baz" |], result)