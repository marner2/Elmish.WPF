namespace Elmish.WPF

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

type TypedBaseBindingData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id when 'id : equality> =
  internal
  | TypedOneWayData of OneWayData<'model, 'bindingModel>
  | TypedOneWayToSourceData of OneWayToSourceData<'model, 'msg, 'bindingModel>
  | TypedOneWaySeqLazyData of OneWaySeqLazyData<'model, 'bindingModel, 'bindingViewModel, 'id>
  | TypedCmdData of CmdData<'model, 'msg>
  | TypedSubModelData of SubModelData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel>
  | TypedSubModelWinData of SubModelWinData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel>
  | TypedSubModelSeqUnkeyedData of SubModelSeqUnkeyedData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel>
  | TypedSubModelSeqKeyedData of SubModelSeqKeyedData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id>
  | TypedSubModelSelectedItemData of SubModelSelectedItemData<'model, 'msg, 'id>

and internal TypedValidationData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id when 'id : equality> =
  { BindingData: TypedBindingData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id>
    Validate: 'model -> string list }

and internal TypedLazyData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id when 'id : equality> =
  { BindingData: TypedBindingData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id>
    Equals: 'model -> 'model -> bool }

and internal TypedAlterMsgStreamData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'dispatchMsg, 'id when 'id : equality> =
 { BindingData: TypedBindingData<'bindingModel, 'bindingMsg, obj, obj, 'bindingViewModel, 'id>
   AlterMsgStream: ('dispatchMsg -> unit) -> 'bindingMsg -> unit
   Get: 'model -> 'bindingModel
   Set: 'dispatchMsg -> 'model -> 'msg }

  member this.CreateFinalDispatch
      (getCurrentModel: unit -> 'model,
       dispatch: 'msg -> unit)
       : 'bindingMsg -> unit =
    let dispatch' (dMsg: 'dispatchMsg) = getCurrentModel () |> this.Set dMsg |> dispatch
    this.AlterMsgStream dispatch'

and TypedBindingData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id when 'id : equality> =
  internal
  | TypedBaseBindingData of TypedBaseBindingData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id>
  | TypedCachingData of TypedBindingData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id>
  | TypedValidationData of TypedValidationData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id>
  | TypedLazyData of TypedLazyData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, 'id>
  | TypedAlterMsgStreamData of TypedAlterMsgStreamData<'model, 'msg, 'bindingModel, 'bindingMsg, 'bindingViewModel, obj, 'id>

module StaticBindings =

  let get getter name =
    let ret: OneWayData<'model, obj> = { Get = getter >> box }
    ret |> OneWayData |> BaseBindingData

  let set setter name =
    let ret: OneWayToSourceData<'model, 'msg, 'a> = { Set = setter }
    ret |> TypedOneWayToSourceData |> TypedBaseBindingData

  let cmd name =
    let ret: CmdData<'model, 'msg> =
      { Exec = (fun _ _ -> ValueNone)
        CanExec = (fun _ _ -> false)
        AutoRequery = false }
    ret |> TypedCmdData |> TypedBaseBindingData

  let subModel name createViewModel =
    let ret: SubModelData<'model, 'msg, 'model, 'msg, 'a> =
      { GetModel = id >> ValueSome
        CreateViewModel = createViewModel
        UpdateViewModel = ignore
        ToMsg = (fun _ msg -> msg) }
    ret |> TypedSubModelData |> TypedBaseBindingData

  let mapModel f =
    let binaryHelper binary x m = binary x (f m)
    let baseCase = function
      | TypedOneWayData d -> TypedOneWayData {
          Get = f >> d.Get
        }
      | TypedOneWayToSourceData d -> TypedOneWayToSourceData {
          Set = binaryHelper d.Set
        }
      | TypedOneWaySeqLazyData d -> TypedOneWaySeqLazyData {
          Get = f >> d.Get
          Map = d.Map
          CreateCollection = d.CreateCollection
          Equals = d.Equals
          GetId = d.GetId
          ItemEquals = d.ItemEquals
        }
      | TypedCmdData d -> TypedCmdData {
          Exec = binaryHelper d.Exec
          CanExec = binaryHelper d.CanExec
          AutoRequery = d.AutoRequery
        }
      | TypedSubModelData d -> TypedSubModelData {
          GetModel = f >> d.GetModel
          CreateViewModel = d.CreateViewModel
          UpdateViewModel = d.UpdateViewModel
          ToMsg = f >> d.ToMsg
        }
      | TypedSubModelWinData d -> TypedSubModelWinData {
          GetState = f >> d.GetState
          CreateViewModel = d.CreateViewModel
          UpdateViewModel = d.UpdateViewModel
          ToMsg = f >> d.ToMsg
          GetWindow = f >> d.GetWindow
          IsModal = d.IsModal
          OnCloseRequested = f >> d.OnCloseRequested
        }
      | TypedSubModelSeqUnkeyedData d -> TypedSubModelSeqUnkeyedData {
          GetModels = f >> d.GetModels
          CreateViewModel = d.CreateViewModel
          CreateCollection = d.CreateCollection
          UpdateViewModel = d.UpdateViewModel
          ToMsg = f >> d.ToMsg
        }
      | TypedSubModelSeqKeyedData d -> TypedSubModelSeqKeyedData {
          GetSubModels = f >> d.GetSubModels
          CreateViewModel = d.CreateViewModel
          CreateCollection = d.CreateCollection
          UpdateViewModel = d.UpdateViewModel
          GetUnderlyingModel = d.GetUnderlyingModel
          ToMsg = f >> d.ToMsg
          GetId = d.GetId
        }
      | TypedSubModelSelectedItemData d -> TypedSubModelSelectedItemData {
          Get = f >> d.Get
          Set = binaryHelper d.Set
          SubModelSeqBindingName = d.SubModelSeqBindingName
        }
    let rec recursiveCase = function
      | TypedBaseBindingData d -> d |> baseCase |> TypedBaseBindingData
      | TypedCachingData d -> d |> recursiveCase |> TypedCachingData
      | TypedValidationData d -> TypedValidationData {
          BindingData = recursiveCase d.BindingData
          Validate = f >> d.Validate
        }
      | TypedLazyData d -> TypedLazyData {
          BindingData = recursiveCase d.BindingData
          Equals = fun a1 a2 -> d.Equals (f a1) (f a2)
        }
      | TypedAlterMsgStreamData d -> TypedAlterMsgStreamData {
          BindingData = d.BindingData
          AlterMsgStream = d.AlterMsgStream
          Get = f >> d.Get
          Set = binaryHelper d.Set
        }
    recursiveCase

type ViewModelBaseHelper<'model, 'msg, 'viewModel when 'viewModel :> ViewModelBaseBase>(baseBase: 'viewModel, args: ViewModelArgs<'model, 'msg>) as this =

  let { initialModel = initialModel
        dispatch = dispatch
        loggingArgs = loggingArgs
      } = args

  let { log = log
        nameChain = nameChain
      } = loggingArgs

  let mutable currentModel = initialModel
  let mutable getBindings = Dictionary<String, VmBinding<'model, 'msg>>()
  let mutable setBindings = Dictionary<String, VmBinding<'model, 'msg>>()

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

  let getBinding name =
    let ret: OneWayData<'model, 'model> = { Get = id }
    ret |> TypedOneWayData

  let setBinding name =
    let ret: OneWayToSourceData<'model, 'msg, 'msg> = { Set = (fun m _ -> m) }
    ret |> TypedOneWayToSourceData

  let cmdBinding name =
    let ret: CmdData<'model, 'msg> =
      { Exec = (fun _ _ -> ValueNone)
        CanExec = (fun _ _ -> false)
        AutoRequery = false }
    ret |> TypedCmdData

  let subModelBinding name createViewModel =
    let ret: SubModelData<'model, 'msg, 'model, 'msg, 'a> =
      { GetModel = id >> ValueSome
        CreateViewModel = createViewModel
        UpdateViewModel = ignore
        ToMsg = (fun _ msg -> msg) }
    ret |> TypedSubModelData

  let initializeGetBindingIfNew name (getter: 'model -> 'a) =
    if getBindings.ContainsKey name |> not then
      let bindingData: OneWayData<'model, 'a> =
        { Get = getter }
      let bindingData2: OneWayData<'model, obj> =
        { Get = bindingData.Get >> box }
      let wrappedBindingData = bindingData2 |> OneWayData |> BaseBindingData
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
      let bindingData: OneWayToSourceData<'model, 'msg, 'model -> 'msg> =
        { Set = (fun setter -> setter) }
      let bindingData2: OneWayToSourceData<'model, 'msg, obj> =
        { Set = unbox<'model -> 'msg> >> bindingData.Set }
      let wrappedBindingData = bindingData2 |> OneWayToSourceData |> BaseBindingData
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
  

  member internal _.CurrentModel : 'model = currentModel

  member internal _.UpdateModel (newModel: 'model) : unit =
    let eventsToRaise =
      getBindings
      |> Seq.collect (fun (Kvp (name, binding)) -> Update(loggingArgs, name).Recursive(currentModel, newModel, dispatch, binding))
      |> Seq.toList
    currentModel <- newModel
    baseBase.RaiseEvents eventsToRaise

and ViewModelBaseBase(loggingArgs: LoggingViewModelArgs, x) as this =

  let { log = log
        nameChain = nameChain
      } = loggingArgs

  let propertyChanged = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()
  let errorsChanged = DelegateEvent<EventHandler<DataErrorsChangedEventArgs>>()

  let raisePropertyChanged name =
    log.LogTrace("[{BindingNameChain}] PropertyChanged {BindingName}", nameChain, name)
    propertyChanged.Trigger(this, PropertyChangedEventArgs name)
  let raiseCanExecuteChanged (cmd: Command) =
    cmd.RaiseCanExecuteChanged ()
  let raiseErrorsChanged name =
    log.LogTrace("[{BindingNameChain}] ErrorsChanged {BindingName}", nameChain, name)
    errorsChanged.Trigger([| this; box <| DataErrorsChangedEventArgs name |])

  let mutable validationErrors = Dictionary<String, String list ref>()

  member _.WAT = ViewModelBaseHelper(this, x).CurrentModel

  member internal _.RaiseEvents(eventsToRaise: UpdateData list) =
    eventsToRaise
    |> Seq.iter (function
      | ErrorsChanged name -> name |> raiseErrorsChanged
      | PropertyChanged name -> name |> raisePropertyChanged
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
      let bindingData: OneWayData<'model, 'a> =
        { Get = getter }
      let bindingData2: OneWayData<'model, obj> =
        { Get = bindingData.Get >> box }
      let wrappedBindingData = bindingData2 |> OneWayData |> BaseBindingData
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
      let bindingData: OneWayToSourceData<'model, 'msg, 'model -> 'msg> =
        { Set = (fun setter -> setter) }
      let bindingData2: OneWayToSourceData<'model, 'msg, obj> =
        { Set = unbox<'model -> 'msg> >> bindingData.Set }
      let wrappedBindingData = bindingData2 |> OneWayToSourceData |> BaseBindingData
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

  member _.getValue(getter, [<CallerMemberName>] ?memberName) =
    memberName
    |> ValueOption.ofOption
    |> ValueOption.bind (fun name -> initializeGetBindingIfNew name getter)
    |> ValueOption.defaultValue Unchecked.defaultof<'a>

  member _.setValue(setter, [<CallerMemberName>] ?memberName) =
    memberName
    |> Option.iter (fun name -> initializeSetBindingIfNew name setter)

  member _.cmd(exec, canExec, autoRequery, [<CallerMemberName>] ?memberName) =
    memberName
    |> Option.bind (fun name -> initializeCmdBindingIfNew name exec canExec autoRequery)
    |> Option.defaultValue null

  member _.subModel(getModel, toMsg, createViewModel, updateViewModel, [<CallerMemberName>] ?memberName) =
    memberName
    |> Option.bind (fun name -> initializeSubModelBindingIfNew name getModel toMsg createViewModel updateViewModel)
    |> Option.defaultValue null

  member _.subModelSeqUnkeyed(getModels, toMsg, createViewModel, updateViewModel, [<CallerMemberName>] ?memberName) =
    memberName
    |> Option.bind (fun name -> initializeSubModelSeqUnkeyedBindingIfNew name getModels toMsg createViewModel updateViewModel)
    |> Option.defaultValue null

  member _.subModelSeqKeyed(getModels, toMsg, getKey, createViewModel, updateViewModel, getUnderlyingModel, [<CallerMemberName>] ?memberName) =
    memberName
    |> Option.bind (fun name -> initializeSubModelSeqKeyedBindingIfNew name getModels toMsg getKey createViewModel updateViewModel getUnderlyingModel)
    |> Option.defaultValue null

  member _.subModelWin(getState, toMsg, getWindow, isModal, onCloseRequested, createViewModel, updateViewModel, [<CallerMemberName>] ?memberName) =
    memberName
    |> Option.bind (fun name -> initializeSubModelWinBindingIfNew name getState toMsg getWindow isModal onCloseRequested createViewModel updateViewModel)
    |> Option.defaultValue null

  member _.getSubModelSelectedItem(seqBinding, getter, [<CallerMemberName>] ?memberName) =
    memberName
    |> Option.bind (fun name -> initializeGetSubModelSelectedItemBindingIfNew name getter seqBinding)
    |> Option.defaultValue null
  
  member _.setSubModelSelectedItem(seqBinding, setter, value, [<CallerMemberName>] ?memberName) =
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
      | ErrorsChanged name -> name |> raiseErrorsChanged
      | PropertyChanged name -> name |> raisePropertyChanged
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

module private BaseHelpers =
  let updateViewModel = fun ((vm: #ViewModelBase<'bindingModel, 'bindingMsg>),m) -> vm.UpdateModel(m)
  let getUnderlyingModel = fun (vm: #ViewModelBase<'bindingModel, 'bindingMsg>) -> vm.CurrentModel

module private BindingHelpers =
  let createViewModel bindings = fun args -> ViewModel<'bindingModel, 'bindingMsg>(args, bindings)
  let updateViewModel = fun ((vm: ViewModel<'bindingModel, 'bindingMsg>),m) -> vm.UpdateModel(m)
  let getUnderlyingModel = fun (vm: ViewModel<'bindingModel, 'bindingMsg>) -> vm.CurrentModel
  
type ViewModelBase<'model, 'msg> with

  member this.subModel (getModel, toMsg, createViewModel, [<CallerMemberName>] ?memberName) =
    this.subModel (getModel, toMsg, createViewModel, BaseHelpers.updateViewModel, ?memberName = memberName)

  member this.subModelBindings (getModel, toMsg, bindings, [<CallerMemberName>] ?memberName) =
    this.subModel (getModel, toMsg, BindingHelpers.createViewModel bindings, BindingHelpers.updateViewModel, ?memberName = memberName)

  member this.subModelSeqUnkeyed (getModels, toMsg, createViewModel, [<CallerMemberName>] ?memberName) =
    this.subModelSeqUnkeyed (getModels, toMsg, createViewModel, BaseHelpers.updateViewModel, ?memberName = memberName)

  member this.subModelSeqUnkeyedBindings (getModels, toMsg, bindings, [<CallerMemberName>] ?memberName) =
    this.subModelSeqUnkeyed (getModels, toMsg, BindingHelpers.createViewModel bindings, BindingHelpers.updateViewModel, ?memberName = memberName)

  member this.subModelSeqKeyed (getModels, getKey, toMsg, createViewModel, [<CallerMemberName>] ?memberName) =
    this.subModelSeqKeyed (getModels, getKey, toMsg, createViewModel, BaseHelpers.updateViewModel, BaseHelpers.getUnderlyingModel, ?memberName = memberName)

  member this.subModelSeqKeyedBindings (getModels, getKey, toMsg, bindings, [<CallerMemberName>] ?memberName) =
    this.subModelSeqKeyed (getModels, getKey, toMsg, BindingHelpers.createViewModel bindings, BindingHelpers.updateViewModel, BindingHelpers.getUnderlyingModel, ?memberName = memberName)

  member this.subModelWin (getState, toMsg, getWindow, isModal, onCloseRequested, createViewModel, [<CallerMemberName>] ?memberName) =
    this.subModelWin (getState, toMsg, getWindow, isModal, onCloseRequested, createViewModel, BaseHelpers.updateViewModel, ?memberName = memberName)
    
  member this.subModelWinBindings (getState, toMsg, getWindow, isModal, onCloseRequested, bindings, [<CallerMemberName>] ?memberName) =
    this.subModelWin (getState, toMsg, getWindow, isModal, onCloseRequested, BindingHelpers.createViewModel bindings, BindingHelpers.updateViewModel, ?memberName = memberName)

module BindingBase =
  module SubModelBase =
    open Binding
    open Binding.SubModel

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    let inline vopt (create: ViewModelArgs<'model, 'msg> -> 'viewModel)
        : string -> Binding<'model voption, 'msg> =
      let subModelData =
        { GetModel = id
          CreateViewModel = create
          UpdateViewModel = fun (vm,m) -> (^viewModel: (member UpdateModel: ^model -> unit) vm, m)
          ToMsg = fun _ -> id }
      subModelData
      |> mapMinorTypes box box box unbox unbox unbox
      |> SubModelData
      |> BaseBindingData
      |> createBinding

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    let inline opt (create: ViewModelArgs<'model, 'msg> -> 'viewModel)
        : string -> Binding<'model option, 'msg> =
      vopt create
      >> mapModel ValueOption.ofOption

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    let inline required (create: ViewModelArgs<'model, 'msg> -> 'viewModel)
        : string -> Binding<'model, 'msg> =
      vopt create
      >> mapModel ValueSome

  module SubModelSeqUnkeyedBase =

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    let inline required (create: ViewModelArgs<'model, 'msg> -> 'viewModel)
        : string -> Binding<'model seq, int * 'msg> =
      BindingData.SubModelSeqUnkeyed.create
        create
        (fun (vm,m) -> (^viewModel: (member UpdateModel: ^model -> unit) vm, m))

  module SubModelSeqKeyedBase =

    /// <summary>
    ///   Creates a binding to a sub-model/component. You typically bind this
    ///   to the <c>DataContext</c> of a <c>UserControl</c> or similar.
    /// </summary>
    /// <param name="create">Returns the static view model for the sub-model.</param>
    /// <param name="getId">Returns the identifier for the model.</param>
    let inline required (create: ViewModelArgs<'model, 'msg> -> 'viewModel) (getId: 'model -> 'id)
        : string -> Binding<'model seq, 'id * 'msg> =
      BindingData.SubModelSeqKeyed.create
        create
        (fun (vm,m) -> (^viewModel: (member UpdateModel: ^model -> unit) vm, m))
        (fun vm -> (^viewModel: (member CurrentModel: ^model) vm))
        getId
