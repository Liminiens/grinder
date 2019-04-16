module Tests

open Xunit
open Grinder
open Command

module Assert =
    let Fail() = Assert.True(false)
    let Success() = Assert.True(true)

[<Fact>]
let ``Usernames and command are extracted from text`` () =
    let expected = ["@hi"; "@hi1"; "@hi2"]
    let text = "@hi  @hi1  @hi2   1test"
    match Command.getUsernamesAndCommand text with
    | InvalidCommand -> 
        Assert.Fail()
    | Command data ->
        Assert.Equal<string>(expected, data.Usernames)
        Assert.Equal<string>("1test", data.Command)

[<Fact>]
let ``Invalid command when there are no usernames`` () =
    let text = "test"
    match Command.getUsernamesAndCommand text with
    | InvalidCommand -> 
        Assert.Success()
    | Command data ->
        Assert.Fail()
        
[<Fact>]
let ``getMinutes returns minutes from text`` () =
    let text = "10 minutes  20 days 1 month"
    match Command.getMinutes text with
    | Some(Minutes(value)) -> 
        Assert.Equal(10, value)
    | None ->
        Assert.Fail()
        
[<Fact>]
let ``getDays returns minutes from text`` () =
    let text = "10 minutes  20 days 1 month"
    match Command.getDays text with
    | Some(Days(value)) -> 
        Assert.Equal(20, value)
    | None ->
        Assert.Fail()
        
[<Fact>]
let ``getMonths returns minutes from text`` () =
    let text = "10 minutes  20 days 1 month"
    match Command.getMonths text with
    | Some(Months(value)) -> 
        Assert.Equal(1, value)
    | None ->
        Assert.Fail()