open Jest;

describe("List", () => {
  open Expect;
  test("first smaller than second one", () => {
    let input = RereactDom.equalizeList([1], [1, 6]);
    let result = ([Some(1), None], [Some(1), Some(6)]);
    expect(input) |> toEqual(result);
  });
  test("sameSize", () => {
    let input = RereactDom.equalizeList([1, 24], [214, 123]);
    let result = ([Some(1), Some(24)], [Some(214), Some(123)]);
    expect(input) |> toEqual(result);
  });
  test("second one smaller than first one", () => {
    let input = RereactDom.equalizeList([1, 24], [214]);
    let result = ([Some(1), Some(24)], [Some(214), None]);
    expect(input) |> toEqual(result);
  });
  test("both empty", () => {
    let input = RereactDom.equalizeList([], []);
    let result = ([], []);
    expect(input) |> toEqual(result);
  });
  test("both empty", () => {
    let input = RereactDom.equalizeList([1, 2, 3, 4], [1, 2]);
    let result = ([Some(1), Some(2), Some(3), Some(4)], [Some(1), Some(2), None, None]);
    expect(input) |> toEqual(result);
  });
});