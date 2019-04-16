module Tests

open Xunit
open Grinder

module Assert =
    let Fail() = Assert.True(false)
    let Success() = Assert.True(true)

[<Fact>]
let ``Usernames and command are extracted from text`` () =
    let expected = ["@hi"; "@hi1"; "@hi2"]
    let text = "@hi  @hi1  @hi2   1test"
    match Control.getUsernamesAndCommand text with
    | InvalidCommand -> 
        Assert.Fail()
    | Command data ->
        Assert.Equal<string>(expected, data.Usernames)
        Assert.Equal<string>("   1test", data.Command)

[<Fact>]
let ``Invalid command when there are no usernames`` () =
    let text = "test"
    match Control.getUsernamesAndCommand text with
    | InvalidCommand -> 
        Assert.Success()
    | Command data ->
        Assert.Fail()