﻿namespace Elmish.WPF

open System
open System.Collections.Generic
open System.ComponentModel
open System.Runtime.CompilerServices
open System.Windows.Input
open Microsoft.Extensions.Logging
open System.Collections.ObjectModel
open System.Windows
open FSharp.Quotations
open FSharp.Quotations.Patterns


type [<AllowNullLiteral>] ViewModelBase<'model,'msg>
      ( args: ViewModelArgs<'model, 'msg>,
        getSender: unit -> obj) =

  let { initialModel = initialModel
        dispatch = dispatch
        loggingArgs = loggingArgs
      } = args

  let { log = log
        nameChain = nameChain
      } = loggingArgs

  let mutable currentModel = initialModel

  let propertyChanged = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()
  let errorsChanged = DelegateEvent<EventHandler<DataErrorsChangedEventArgs>>()

  let mutable getBindings = Dictionary<String, VmBinding<'model, 'msg>>()
  let mutable setBindings = Dictionary<String, VmBinding<'model, 'msg>>()
  let mutable validationErrors = Dictionary<String, String list ref>()

  let raisePropertyChanged name =
    log.LogTrace("[{BindingNameChain}] PropertyChanged {BindingName}", nameChain, name)
    propertyChanged.Trigger(getSender (), PropertyChangedEventArgs name)
  let raiseCanExecuteChanged (cmd: Command) =
    cmd.RaiseCanExecuteChanged ()
  let raiseErrorsChanged name =
    log.LogTrace("[{BindingNameChain}] ErrorsChanged {BindingName}", nameChain, name)
    errorsChanged.Trigger([| getSender (); box <| DataErrorsChangedEventArgs name |])
  let getFunctionsForSubModelSelectedItem name =
    getBindings
    |> Dictionary.tryFind name
    |> function
      | Some b ->
        match FuncsFromSubModelSeqKeyed().Recursive(b) with
        | Some x ->
          Some x
        | None ->
          log.LogError("SubModelSelectedItem binding referenced binding {SubModelSeqBindingName} but it is not a SubModelSeq binding", name)
          None
      | None ->
        log.LogError("SubModelSelectedItem binding referenced binding {SubModelSeqBindingName} but no binding was found with that name", name)
        None

  let initializeGetBindingIfNew name (getter: 'model -> 'a) =
    if getBindings.ContainsKey name |> not then
      let bindingData: OneWayData<'model, obj> =
        { Get = getter >> box }
      let wrappedBindingData = bindingData |> OneWayData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> getBindings.Add(name, binding))
    let binding = Get(nameChain).Recursive(currentModel, getBindings.Item name)
    match binding with
    | Ok o -> o |> unbox<'a> |> ValueSome
    | Error error -> log.LogError("Wrong binding type found for {name}, should be BaseVmBinding, found {foundBinding}", name, error); ValueNone
      
  let initializeSetBindingIfNew name (setter: 'model -> 'msg) =
    if setBindings.ContainsKey name |> not then
      let bindingData: OneWayToSourceData<'model, 'msg, obj> =
        { Set = (fun setter -> setter |> unbox<'model -> 'msg>) }
      let wrappedBindingData = bindingData |> OneWayToSourceData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> setBindings.Add(name, binding))
    let didSet = Set(setter |> box).Recursive(currentModel, setBindings.Item name)
    if not didSet then
      log.LogError("Failed to set binding {name}", name)

  let initializeCmdBindingIfNew name exec canExec autoRequery =
    if getBindings.ContainsKey name |> not then
      let bindingData =
        { Exec = exec
          CanExec = canExec
          AutoRequery = autoRequery }
      let wrappedBindingData = bindingData |> CmdData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> getBindings.Add(name, binding))
      
    let binding = Get(nameChain).Recursive(currentModel, getBindings.Item name)
    match binding with
    | Ok o -> o |> unbox<ICommand> |> Some
    | Error error -> log.LogError("Wrong binding type found for {name}, should be BaseVmBinding, found {foundBinding}", name, error); None

  let initializeSubModelBindingIfNew
    name
    (getModel: 'model -> 'bindingModel voption)
    (toMsg: 'model -> 'bindingMsg -> 'msg)
    (createViewModel: ViewModelArgs<'bindingModel, 'bindingMsg> -> 'bindingViewModel)
    (updateViewModel: 'bindingViewModel * 'bindingModel -> unit) =
    if getBindings.ContainsKey name |> not then
      let bindingData =
        { GetModel = getModel
          ToMsg = toMsg
          CreateViewModel = createViewModel
          UpdateViewModel = updateViewModel }
      let bindingData2 = Binding.SubModel.mapMinorTypes box box box unbox unbox unbox bindingData
      let wrappedBindingData = bindingData2 |> SubModelData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> getBindings.Add(name, binding))
      
    let binding = Get(nameChain).Recursive(currentModel, getBindings.Item name)
    match binding with
    | Ok o -> o |> unbox<'bindingViewModel> |> Some
    | Error error -> log.LogError("Wrong binding type found for {name}, should be BaseVmBinding, found {foundBinding}", name, error); None

  let initializeSubModelSeqUnkeyedBindingIfNew
    name
    (getModels: 'model -> 'bindingModel seq)
    (toMsg: 'model -> int * 'bindingMsg -> 'msg)
    (createViewModel: ViewModelArgs<'bindingModel, 'bindingMsg> -> 'bindingViewModel)
    (updateViewModel: 'bindingViewModel * 'bindingModel -> unit) =
    if getBindings.ContainsKey name |> not then
      let bindingData =
        { GetModels = getModels
          ToMsg = toMsg
          CreateViewModel = createViewModel
          CreateCollection = ObservableCollection >> CollectionTarget.create
          UpdateViewModel = updateViewModel }
      let bindingData2 = BindingData.SubModelSeqUnkeyed.box bindingData
      let wrappedBindingData = bindingData2 |> SubModelSeqUnkeyedData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> getBindings.Add(name, binding))

    let binding = Get(nameChain).Recursive(currentModel, getBindings.Item name)
    match binding with
    | Ok o -> o |> unbox<ObservableCollection<'bindingViewModel>> |> Some
    | Error error -> log.LogError("Wrong binding type found for {name}, should be BaseVmBinding, found {foundBinding}", name, error); None

  let initializeSubModelSeqKeyedBindingIfNew
    name
    (getModels: 'model -> 'bindingModel seq)
    (getKey: 'bindingModel -> 'a)
    (toMsg: 'model -> 'a * 'bindingMsg -> 'msg)
    (createViewModel: ViewModelArgs<'bindingModel, 'bindingMsg> -> 'bindingViewModel)
    (updateViewModel: 'bindingViewModel * 'bindingModel -> unit)
    (getUnderlyingModel: 'bindingViewModel -> 'bindingModel) =
    if getBindings.ContainsKey name |> not then
      let bindingData =
        { GetSubModels = getModels
          ToMsg = toMsg
          CreateViewModel = createViewModel
          CreateCollection = ObservableCollection >> CollectionTarget.create
          GetUnderlyingModel = getUnderlyingModel
          UpdateViewModel = updateViewModel
          GetId = getKey }
      let bindingData2 = BindingData.SubModelSeqKeyed.box bindingData
      let wrappedBindingData = bindingData2 |> SubModelSeqKeyedData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> getBindings.Add(name, binding))

    let binding = Get(nameChain).Recursive(currentModel, getBindings.Item name)
    match binding with
    | Ok o -> o |> unbox<ObservableCollection<'bindingViewModel>> |> Some
    | Error error -> log.LogError("Wrong binding type found for {name}, should be BaseVmBinding, found {foundBinding}", name, error); None
    
  let initializeSubModelWinBindingIfNew
    name
    (getState: 'model -> WindowState<'bindingModel>)
    (toMsg: 'model -> 'bindingMsg -> 'msg)
    (getWindow: 'model -> Elmish.Dispatch<'msg> -> Window)
    (isModal: bool)
    (onCloseRequested: 'model -> 'msg voption)
    (createViewModel: ViewModelArgs<'bindingModel, 'bindingMsg> -> 'bindingViewModel)
    (updateViewModel: 'bindingViewModel * 'bindingModel -> unit) =
    if getBindings.ContainsKey name |> not then
      let bindingData: SubModelWinData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel> =
        { GetState = getState
          ToMsg = toMsg
          CreateViewModel = createViewModel
          UpdateViewModel = updateViewModel
          GetWindow = getWindow
          IsModal = isModal
          OnCloseRequested = onCloseRequested }
      let bindingData2 = BindingData.SubModelWin.mapMinorTypes box box box unbox unbox unbox bindingData
      let wrappedBindingData = bindingData2 |> SubModelWinData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> getBindings.Add(name, binding))
      
    let binding = Get(nameChain).Recursive(currentModel, getBindings.Item name)
    match binding with
    | Ok o -> o |> unbox<'bindingViewModel> |> Some
    | Error error -> log.LogError("Wrong binding type found for {name}, should be BaseVmBinding, found {foundBinding}", name, error); None
    
  let initializeGetSubModelSelectedItemBindingIfNew
    name
    (get: 'model -> 'a voption)
    (seqBinding: Expr<ObservableCollection<'bindingViewModel>>) =
    if getBindings.ContainsKey name |> not then
      let subModelSeqBindingName =
        let rec watwat b =
          match b with
          | PropertyGet(Some _, propInfo, [ ]) -> propInfo.Name
          | x -> failwithf "Expected a property getter, got a %A" x
        watwat seqBinding
      let _ = eval seqBinding
      let bindingData =
        { Get = get
          Set = (fun _ _ -> failwith "should not be set")
          SubModelSeqBindingName = subModelSeqBindingName }
      let bindingData2 =
        { Get = bindingData.Get >> ValueOption.map box
          Set = ValueOption.map unbox >> bindingData.Set
          SubModelSeqBindingName = bindingData.SubModelSeqBindingName }
      let wrappedBindingData = bindingData2 |> SubModelSelectedItemData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> getBindings.Add(name, binding))

    let binding = Get(nameChain).Recursive(currentModel, getBindings.Item name)
    match binding with
    | Ok o -> o |> unbox<'bindingViewModel> |> Some
    | Error error -> log.LogError("Wrong binding type found for {name}, should be BaseVmBinding, found {foundBinding}", name, error); None
    
  let initializeSetSubModelSelectedItemBindingIfNew
    name
    (set: 'a voption -> 'model -> 'msg)
    (seqBinding: Expr<ObservableCollection<'bindingViewModel>>)
    (v: 'bindingViewModel)=
    if setBindings.ContainsKey name |> not then
      let subModelSeqBindingName =
        let rec watwat b =
          match b with
          | PropertyGet(Some _, propInfo, [ ]) -> propInfo.Name
          | x -> failwithf "Expected a property getter, got a %A" x
        watwat seqBinding
      let _ = eval seqBinding
      let bindingData =
        { Get = (fun _ -> failwith "should not get")
          Set = set
          SubModelSeqBindingName = subModelSeqBindingName }
      let bindingData2 =
        { Get = bindingData.Get >> ValueOption.map box
          Set = ValueOption.map unbox >> bindingData.Set
          SubModelSeqBindingName = bindingData.SubModelSeqBindingName }
      let wrappedBindingData = bindingData2 |> SubModelSelectedItemData |> BaseBindingData
      let binding =
        Initialize(loggingArgs, name, getFunctionsForSubModelSelectedItem)
          .Recursive(initialModel, dispatch, (fun () -> currentModel), wrappedBindingData)
      do binding |> Option.iter (fun binding -> setBindings.Add(name, binding))
    let didSet = Set(v).Recursive(currentModel, setBindings.Item name)
    if not didSet then
      log.LogError("Failed to set binding {name}", name)

  member _.getValue(getter: 'model -> 'a, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> ValueOption.ofOption
    |> ValueOption.bind (fun name -> initializeGetBindingIfNew name getter)
    |> ValueOption.defaultValue Unchecked.defaultof<'a>

  member _.setValue(setter: 'model -> 'msg, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> Option.iter (fun name -> initializeSetBindingIfNew name setter)

  member _.cmd(exec: obj -> 'model -> 'msg voption, canExec: obj -> 'model -> bool, autoRequery: bool, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> Option.bind (fun name -> initializeCmdBindingIfNew name exec canExec autoRequery)
    |> Option.defaultValue null

  member _.subModel(getModel, toMsg, createViewModel, updateViewModel, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> Option.bind (fun name -> initializeSubModelBindingIfNew name getModel toMsg createViewModel updateViewModel)
    |> Option.defaultValue null

  member _.subModelSeqUnkeyed(getModels: 'model -> 'bindingModel seq, toMsg, createViewModel, updateViewModel, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> Option.bind (fun name -> initializeSubModelSeqUnkeyedBindingIfNew name getModels toMsg createViewModel updateViewModel)
    |> Option.defaultValue null

  member _.subModelSeqKeyed(getModels: 'model -> 'bindingModel seq, toMsg, getKey, createViewModel, updateViewModel, getUnderlyingModel, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> Option.bind (fun name -> initializeSubModelSeqKeyedBindingIfNew name getModels toMsg getKey createViewModel updateViewModel getUnderlyingModel)
    |> Option.defaultValue null

  member _.subModelWin(getState, toMsg, getWindow, isModal, onCloseRequested, createViewModel, updateViewModel, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> Option.bind (fun name -> initializeSubModelWinBindingIfNew name getState toMsg getWindow isModal onCloseRequested createViewModel updateViewModel)
    |> Option.defaultValue null

  member _.getSubModelSelectedItem(seqBinding, getter, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> Option.bind (fun name -> initializeGetSubModelSelectedItemBindingIfNew name getter seqBinding)
    |> Option.defaultValue null
  
  member _.setSubModelSelectedItem(seqBinding, setter, value, [<CallerMemberName>] ?memberName: string) =
    memberName
    |> Option.iter (fun name -> initializeSetSubModelSelectedItemBindingIfNew name setter seqBinding value)

  member internal _.CurrentModel : 'model = currentModel

  member internal _.UpdateModel (newModel: 'model) : unit =
    let eventsToRaise =
      getBindings
      |> Seq.collect (fun (Kvp (name, binding)) -> Update(loggingArgs, name).Recursive(currentModel, newModel, dispatch, binding))
      |> Seq.toList
    currentModel <- newModel
    eventsToRaise
    |> List.iter (function
      | ErrorsChanged name -> raiseErrorsChanged name
      | PropertyChanged name -> raisePropertyChanged name
      | CanExecuteChanged cmd -> cmd |> raiseCanExecuteChanged)

  interface INotifyPropertyChanged with
    [<CLIEvent>]
    member _.PropertyChanged = propertyChanged.Publish

  interface INotifyDataErrorInfo with
    [<CLIEvent>]
    member _.ErrorsChanged = errorsChanged.Publish
    member _.HasErrors =
      // WPF calls this too often, so don't log https://github.com/elmish/Elmish.WPF/issues/354
      validationErrors
      |> Seq.map (fun (Kvp(_, errors)) -> errors.Value)
      |> Seq.filter (not << List.isEmpty)
      |> (not << Seq.isEmpty)
    member _.GetErrors name =
      let name = name |> Option.ofObj |> Option.defaultValue "<null>" // entity-level errors are being requested when given null or ""  https://docs.microsoft.com/en-us/dotnet/api/system.componentmodel.inotifydataerrorinfo.geterrors#:~:text=null%20or%20Empty%2C%20to%20retrieve%20entity-level%20errors
      log.LogTrace("[{BindingNameChain}] GetErrors {BindingName}", nameChain, name)
      validationErrors
      |> IReadOnlyDictionary.tryFind name
      |> Option.map (fun errors -> errors.Value)
      |> Option.defaultValue []
      |> (fun x -> upcast x)

type ViewModelBase<'model, 'msg> with
  member this.subModel (getModel: 'model -> 'bindingModel voption, toMsg, createViewModel: ViewModelArgs<'bindingModel, 'bindingMsg> -> #ViewModelBase<'bindingModel, 'bindingMsg>, [<CallerMemberName>] ?memberName: string) =
    this.subModel (getModel, toMsg, createViewModel, (fun (vm,m) -> vm.UpdateModel(m)), ?memberName = memberName)

  member this.subModelBindings (getModel: 'model -> 'bindingModel voption, toMsg, bindings, [<CallerMemberName>] ?memberName: string) =
    this.subModel (getModel, toMsg, (fun args -> ViewModel<'bindingModel, 'bindingMsg>(args, bindings)), (fun (vm,m) -> vm.UpdateModel(m)), ?memberName = memberName)

  member this.subModelSeqUnkeyed (getModels: 'model -> 'bindingModel seq, toMsg, createViewModel: ViewModelArgs<'bindingModel, 'bindingMsg> -> #ViewModelBase<'bindingModel, 'bindingMsg>, [<CallerMemberName>] ?memberName: string) =
    this.subModelSeqUnkeyed (getModels, toMsg, createViewModel, (fun (vm,m) -> vm.UpdateModel(m)), ?memberName = memberName)

  member this.subModelSeqUnkeyedBindings (getModels: 'model -> 'bindingModel seq, toMsg, bindings, [<CallerMemberName>] ?memberName: string) =
    this.subModelSeqUnkeyed (getModels, toMsg, (fun args -> ViewModel<'bindingModel, 'bindingMsg>(args, bindings)), (fun (vm,m) -> vm.UpdateModel(m)), ?memberName = memberName)

  member this.subModelSeqKeyed (getModels: 'model -> 'bindingModel seq, getKey, toMsg, createViewModel: ViewModelArgs<'bindingModel, 'bindingMsg> -> #ViewModelBase<'bindingModel, 'bindingMsg>, [<CallerMemberName>] ?memberName: string) =
    this.subModelSeqKeyed (getModels, getKey, toMsg, createViewModel, (fun (vm,m) -> vm.UpdateModel(m)), (fun vm -> vm.CurrentModel), ?memberName = memberName)

  member this.subModelSeqKeyedBindings (getModels: 'model -> 'bindingModel seq, getKey, toMsg, bindings, [<CallerMemberName>] ?memberName: string) =
    this.subModelSeqKeyed (getModels, getKey, toMsg, (fun args -> ViewModel<'bindingModel, 'bindingMsg>(args, bindings)), (fun (vm,m) -> vm.UpdateModel(m)), (fun vm -> vm.CurrentModel), ?memberName = memberName)

  member this.subModelWin (getState, toMsg, getWindow, isModal, onCloseRequested, createViewModel: ViewModelArgs<'bindingModel, 'bindingMsg> -> #ViewModelBase<'bindingModel, 'bindingMsg>, [<CallerMemberName>] ?memberName: string) =
    this.subModelWin (getState, toMsg, getWindow, isModal, onCloseRequested, createViewModel, (fun (vm,m) -> vm.UpdateModel(m)), ?memberName = memberName)
    
  member this.subModelWinBindings (getState, toMsg, getWindow, isModal, onCloseRequested, bindings, [<CallerMemberName>] ?memberName: string) =
    this.subModelWin (getState, toMsg, getWindow, isModal, onCloseRequested, (fun args -> ViewModel<'bindingModel, 'bindingMsg>(args, bindings)), (fun (vm,m) -> vm.UpdateModel(m)), ?memberName = memberName)

module BindingBase =
  module SubModelBase =
    open Binding
    open Binding.SubModel

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    let vopt (create: ViewModelArgs<'model, 'msg> -> #ViewModelBase<'model,'msg>)
        : string -> Binding<'model voption, 'msg> =
      { GetModel = id
        CreateViewModel = create
        UpdateViewModel = fun (vm,m) -> vm.UpdateModel(m)
        ToMsg = fun _ -> id }
      |> mapMinorTypes box box box unbox unbox unbox
      |> SubModelData
      |> BaseBindingData
      |> createBinding

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    let opt (create: ViewModelArgs<'model, 'msg> -> #ViewModelBase<'model,'msg>)
        : string -> Binding<'model option, 'msg> =
      vopt create
      >> mapModel ValueOption.ofOption

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    let required (create: ViewModelArgs<'model, 'msg> -> #ViewModelBase<'model,'msg>)
        : string -> Binding<'model, 'msg> =
      vopt create
      >> mapModel ValueSome

  module SubModelSeqUnkeyedBase =

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    let required (create: ViewModelArgs<'model, 'msg> -> #ViewModelBase<'model,'msg>)
        : string -> Binding<'model seq, int * 'msg> =
      BindingData.SubModelSeqUnkeyed.create
        create
        (fun (vm,m) -> vm.UpdateModel(m))

  module SubModelSeqKeyedBase =

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    /// <param name="getId">Returns the identifier for the model.</param>
    let required (create: ViewModelArgs<'model, 'msg> -> #ViewModelBase<'model,'msg>) (getId: 'model -> 'id)
        : string -> Binding<'model seq, 'id * 'msg> =
      BindingData.SubModelSeqKeyed.create
        create
        (fun (vm,m) -> vm.UpdateModel(m))
        (fun vm -> vm.CurrentModel)
        getId