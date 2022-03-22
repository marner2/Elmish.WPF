﻿module Elmish.WPF.ViewModel

open Microsoft.Extensions.Logging.Abstractions

/// Creates a design-time view model using the given model and bindings.
let designInstance (model: 'model) (bindings: Binding<'model, 'msg> list) =
  let args = ViewModelArgs.fake model

  ViewModel(args, bindings) |> box