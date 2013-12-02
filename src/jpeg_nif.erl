%% Copyright (c) 2012 Dmitry Geurkov (troydm) d.geurkov@gmail.com
%% 
%% This file is part of erl_ipcam.
%% ipcam motion detection and mjpeg data gathering software written in Erlang
%% 
%% erl_ipcam is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(jpeg_nif).
-author('Dmitry Geurkov d.geurkov@gmail.com').

-export([load/1,compare/6]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./jpeg_nif", 0).

load(_Binary) ->
    exit(nif_library_not_loaded).

compare(_B1,_B2,_Pos,_MinPos,_Step,_Min) ->
    exit(nif_library_not_loaded).

