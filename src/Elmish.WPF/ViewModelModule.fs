module Elmish.WPF.ViewModel

open System.Runtime.CompilerServices

/// Creates a design-time view model using the given model and bindings.
let designInstance (model: 'model) (bindings: Binding<'model, 'msg, obj> list) =
  let args = ViewModelArgs.simple model

  ViewModel(args, bindings) |> box


module internal Static =
  type StaticHelper<'model, 'msg>(args: ViewModelArgs<'model, 'msg>) =
    let {
      initialModel = initialModel
      dispatch = dispatch
      loggingArgs = {
        nameChain = nameChain
      }
    } = args
    let mutable currentModel = initialModel


    member _.getValue (bindingFunc: string -> Binding<'model, 'msg, 'a>, [<CallerMemberName>] ?memberName: string) =
      option {
        let! name = memberName
        let binding = bindingFunc name
        let binding = BindingData.Binding.mapVm box unbox binding
        let! b =
          Initialize(args.loggingArgs, binding.Name, failwith "nope")
            .Recursive(currentModel, dispatch, (fun () -> currentModel), binding.Data)
        let c = Get(nameChain).Recursive(currentModel, b)
        let! d =
          match c with
          | Ok x -> Some x
          | Error _ -> None
        return d |> unbox
      } |> Option.defaultValue null

    member _.setValue (value, [<CallerMemberName>] ?memberName: string) =
      fun (bindingFunc: string -> Binding<'model, 'msg, 'a>) ->
        option {
          let! name = memberName
          let binding = bindingFunc name
          let binding = BindingData.Binding.mapVm box unbox binding
          let! b =
            Initialize(args.loggingArgs, binding.Name, failwith "nope")
              .Recursive(currentModel, dispatch, (fun () -> currentModel), binding.Data)
          let _c = Set(value |> box).Recursive(currentModel, b)
          return ()
        } |> Option.defaultValue ()



  type TestVm(args) =
    let helper = StaticHelper<int16, int32>(args)

    member _.Test
      with get() = Binding.OneWay.id >> Binding.mapModel id >> Binding.addValidation (fun _x -> []) |> helper.getValue
      and set(v) = Binding.OneWayToSource.id |> helper.setValue v