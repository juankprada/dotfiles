%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxClipboardTextEvent).
-moduledoc """
Functions for wxClipboardTextEvent class

This class represents the events generated by a control (typically a
`m:wxTextCtrl` but other windows can generate these events as well) when its
content gets copied or cut to, or pasted from the clipboard.

There are three types of corresponding events `wxEVT_TEXT_COPY`,
`wxEVT_TEXT_CUT` and `wxEVT_TEXT_PASTE`.

If any of these events is processed (without being skipped) by an event handler,
the corresponding operation doesn't take place which allows preventing the text
from being copied from or pasted to a control. It is also possible to examine
the clipboard contents in the PASTE event handler and transform it in some way
before inserting in a control - for example, changing its case or removing
invalid characters.

Finally notice that a CUT event is always preceded by the COPY event which makes
it possible to only process the latter if it doesn't matter if the text was
copied or cut.

Note: These events are currently only generated by `m:wxTextCtrl` in wxGTK and
wxOSX but are also generated by `m:wxComboBox` without wxCB_READONLY style in
wxMSW.

See: `m:wxClipboard`

This class is derived (and can use functions) from: `m:wxCommandEvent`
`m:wxEvent`

wxWidgets docs:
[wxClipboardTextEvent](https://docs.wxwidgets.org/3.1/classwx_clipboard_text_event.html)

## Events

Use `wxEvtHandler:connect/3` with
[`wxClipboardTextEventType`](`t:wxClipboardTextEventType/0`) to subscribe to
events of this type.
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxClipboardTextEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxClipboardTextEventType() :: 'command_text_copy' | 'command_text_cut' | 'command_text_paste'.
-export_type([wxClipboardTextEvent/0, wxClipboardText/0, wxClipboardTextEventType/0]).
%% @hidden
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

 %% From wxCommandEvent
%% @hidden
-doc false.
setString(This,String) -> wxCommandEvent:setString(This,String).
%% @hidden
-doc false.
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
%% @hidden
-doc false.
isSelection(This) -> wxCommandEvent:isSelection(This).
%% @hidden
-doc false.
isChecked(This) -> wxCommandEvent:isChecked(This).
%% @hidden
-doc false.
getString(This) -> wxCommandEvent:getString(This).
%% @hidden
-doc false.
getSelection(This) -> wxCommandEvent:getSelection(This).
%% @hidden
-doc false.
getInt(This) -> wxCommandEvent:getInt(This).
%% @hidden
-doc false.
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
%% @hidden
-doc false.
getClientData(This) -> wxCommandEvent:getClientData(This).
 %% From wxEvent
%% @hidden
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
-doc false.
skip(This) -> wxEvent:skip(This).
%% @hidden
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
-doc false.
getId(This) -> wxEvent:getId(This).
