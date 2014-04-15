namespace Neo4j.FSharp.Tests

open System
open System.Numerics
open Neo4j.FSharp
open Xunit
open Swensen.Unquote

module PropertyExtractionTests =

    type SampleRecord = {
        IntProp: int
        StrProp: string
        DateProp: DateTime
        DtoProp: DateTimeOffset
        FloatProp: float
        BoolProp: bool
        GuidProp: Guid
        BigIntProp: BigInteger
    }

    type SampleClass() =
        member val IntProp = 0 with get, set
        member val StrProp = "" with get, set
        member val DateProp = DateTime.MinValue with get, set
        member val DtoProp = DateTimeOffset.MinValue with get, set
        member val FloatProp = 0.0 with get, set
        member val BoolProp = false with get, set
        member val GuidProp = Guid.Empty with get, set
        member val BigIntProp = 0I with get, set

    type ClassWithNonPublic() =
        member val PublicProp = 0 with get, set
        member val internal InternalProp = 0 with get, set

    type ClassWithIndexed() =
        member val NonIndexed = 0 with get, set
        member x.IndexedProp with get (index:int) = 42

    type ClassWithStatic() =
        static member val StaticProp = 83 with get, set
        member val NonStaticProp = 34 with get, set

    [<Fact>]
    let ``Props: Can pass through an array of the right type`` () =
        let values = [| "a", box 1; "b", box 2; "c", box 3; "d", box 4 |]
        let _, props = PropertyExtractor.getProperties values
        test <@ props = values @>

    [<Fact>]
    let ``Props: Sequence of the right type is converted to array`` () =
        let values = [ "a", box 1; "b", box 2; "c", box 3; "d", box 4 ] |> Seq.ofList
        let _, props = PropertyExtractor.getProperties values
        test <@ props = [| "a", box 1; "b", box 2; "c", box 3; "d", box 4 |] @>

    [<Fact>]
    let ``Props: Sequence of the wrong type has no properties`` () =
        let values = seq { 1 .. 100 }
        let name, props = PropertyExtractor.getProperties values
        test <@ props.Length = 0 @>

    [<Fact>]
    let ``Props: Can serialize a Record`` () =
        let values = { 
            IntProp = 13
            StrProp = "foo"
            DateProp = DateTime(2063, 4, 5)
            DtoProp = DateTimeOffset(DateTime(2063, 4, 5), TimeSpan.FromHours(-6.0))
            FloatProp = 95.2
            BoolProp = true
            GuidProp = Guid("e8ce994d-74f2-492e-8e71-dbce8581622d")
            BigIntProp = 295712958751897189I
        }
        let expected = [| ("IntProp", box values.IntProp); ("StrProp", box values.StrProp); ("DateProp", box values.DateProp);
                          ("DtoProp", box values.DtoProp); ("FloatProp", box values.FloatProp);
                          ("BoolProp", box values.BoolProp); ("GuidProp", box values.GuidProp);
                          ("BigIntProp", box values.BigIntProp) |]

        let name, props = PropertyExtractor.getProperties values
        test <@ name = "SampleRecord" @>
        test <@ props = expected @>

    [<Fact>]
    let ``Props: Can serialize a Class`` () =
        let values = 
            SampleClass(
                IntProp = 13,
                StrProp = "foo",
                DateProp = DateTime(2063, 4, 5),
                DtoProp = DateTimeOffset(DateTime(2063, 4, 5), TimeSpan.FromHours(-6.0)),
                FloatProp = 95.2,
                BoolProp = true,
                GuidProp = Guid("e8ce994d-74f2-492e-8e71-dbce8581622d"),
                BigIntProp = 295712958751897189I)
        let expected = [| ("IntProp", box values.IntProp); ("StrProp", box values.StrProp); ("DateProp", box values.DateProp);
                          ("DtoProp", box values.DtoProp); ("FloatProp", box values.FloatProp);
                          ("BoolProp", box values.BoolProp); ("GuidProp", box values.GuidProp);
                          ("BigIntProp", box values.BigIntProp) |]

        let name, props = PropertyExtractor.getProperties values
        test <@ name = "SampleClass" @>
        test <@ props = expected @>

    [<Fact>]
    let ``Props: non-public properties are omitted`` () =
        let values = ClassWithNonPublic(PublicProp = 97, InternalProp = 93)
        let expected = [| "PublicProp", box 97 |]
        let name, props = PropertyExtractor.getProperties values
        test <@ name = "ClassWithNonPublic" @>
        test <@ props = expected @>

    [<Fact>]
    let ``Props: indexed properties are omitted`` () =
        let values = ClassWithIndexed(NonIndexed = 45)
        let expected = [| "NonIndexed", box 45 |]
        let name, props = PropertyExtractor.getProperties values
        test <@ name = "ClassWithIndexed" @>
        test <@ props = expected @>

    [<Fact>]
    let ``Props: static properties are omitted`` () =
        let values = ClassWithStatic(NonStaticProp = 73)
        let expected = [| "NonStaticProp", box 73 |]
        let name, props = PropertyExtractor.getProperties values
        test <@ name = "ClassWithStatic" @>
        test <@ props = expected @>

    [<Fact>]
    let ``Props: Tuples are rejected`` () =
        let values = 1, 3, "blue"
        raises<TypeInitializationException> <@ PropertyExtractor.getProperties values |> ignore @>

    [<Fact>]
    let ``Props: Discriminated Unions are rejected`` () =
        let values = Some 1
        raises<TypeInitializationException> <@ PropertyExtractor.getProperties values |> ignore @>

