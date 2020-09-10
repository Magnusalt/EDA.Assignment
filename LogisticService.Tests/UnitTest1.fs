module LogisticService.Tests

open NUnit.Framework
open LogisticService.RoutePlan

[<SetUp>]
let Setup () = ()

let validData =
    [ TestCaseData([| "B" |]).Returns(5)
      TestCaseData([| "A" |]).Returns(5)
      TestCaseData([| "A"; "B" |]).Returns(5)
      TestCaseData([| "A";"B";"B" |]).Returns(7)
      TestCaseData([| "A";"A";"B";"A";"B";"B";"A";"B" |]).Returns(29) 
      TestCaseData([| "A";"A";"A";"A";"B";"B";"B";"B" |]).Returns(29) 
      TestCaseData([| "B";"B";"B";"B";"A";"A";"A";"A" |]).Returns(49) 
      ]

[<Test>]
[<TestCaseSourceAttribute("validData")>]
let ``plan with valid input returns correct time `` input = plan input
