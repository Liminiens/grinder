namespace Grinder

open Xunit

[<RequireQualifiedAccess>]    
module Assert =
    let Fail() = Assert.True(false)
    
    let FailWithMessage text = Assert.True(false, text)
    
    let Success() = Assert.True(true)
    
[<AutoOpen>]
module Funogram =
    open Funogram.Types    

    let defaultUser =
         { Id = -1L; IsBot = false; FirstName = null; LastName = None; Username = None; LanguageCode = None }