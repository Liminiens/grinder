namespace Grinder

open Xunit

[<RequireQualifiedAccess>]    
module Assert =
    let Fail() = Assert.True(false)
    
    let FailWithMessage text = Assert.True(false, text)
    
    let Success() = Assert.True(true)