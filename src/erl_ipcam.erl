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

-module(erl_ipcam).
-author('Dmitry Geurkov (troydm) d.geurkov@gmail.com').

-export([start/0,start/1,connect/5,start_process/11,comparator/3,get_compare_result/12,image_writer/5]).

start() ->
    start(["config"]).

start([Config|_]) ->
    case file:consult(Config) of
	{ok, [H|_]} ->
	    io:format("Config loaded: ~s ~n",[Config]),
	    [start_camera(Camera) || Camera <- H];
	{error,Reason} ->
	    io:format("Couldn't load: ~s (~s) ~n",[Config,Reason]),
	    halt(1)
    end.

start_camera({Camera,{host,Host},{port,Port},{uri,URI},{debug,Debug},{frames,Frames},{skip_frames,SkipFrames},{buffer,Buffer},{alert,Alert},{motion,Motion},{step,Step},{min,Min},{processes,Processes},{path,Path},{command,Command}}) ->
    io:format("Starting capturing ~p~n",[Camera]),
    Processor = spawn(?MODULE,start_process,[atom_to_list(Camera),Debug,SkipFrames,Motion,Step,Min,Buffer,Alert,Processes,Path,Command]),
    spawn(?MODULE,connect,[Host,Port,URI,Frames,Processor]).

start_process(Camera,Debug,SkipFrames,Motion,Step,Min,Buffer,Alert,Processes,Path,Command) ->
    ImageWriter = spawn(?MODULE,image_writer,[Path,Command,"",0,[]]),
    ResultProcessor = spawn(?MODULE,get_compare_result,[Camera,Debug,self(),Processes,Motion,Alert,0,Processes,os:timestamp(),0,0,0]),
    Comparators = [spawn(?MODULE,comparator,[ResultProcessor,Step,Min]) || _ <- lists:seq(1,Processes)],
    process(Camera,SkipFrames,Motion,Buffer,Alert,Comparators,Processes,Path,Command,ImageWriter,[],[]).

process(Camera,SkipFrames,Motion,Buffer,Alert,Comparators,Processes,Path,Command,ImageWriter,PrevImg,Result) ->
    receive
	{imgs,Imgs} ->
	    ImgData = load_jpeg_data(SkipFrames,Imgs,PrevImg),
	    if ImgData =/= [] ->
	    	    {W,H,_} = hd(ImgData),
	    	    ImgByteSize = W*H*3,
	    	    CompareSize = ImgByteSize/Processes,
	    	    send_to_comparator(ImgData,Comparators,ImgByteSize-1,CompareSize)
	    end,
	    process(Camera,SkipFrames,Motion,Buffer,Alert,Comparators,Processes,Path,Command,ImageWriter,if ImgData =/= [] -> [hd(ImgData)]; true -> [] end,[Imgs|lists:sublist(Result,Buffer)]);
	{alert,CurrentAlert} ->
	    ImageWriter ! {imgs,CurrentAlert,Result},
	    process(Camera,SkipFrames,Motion,Buffer,Alert,Comparators,Processes,Path,Command,ImageWriter,PrevImg,[])	    
    end.

image_writer(Path,Command,AlertPath,ImageIndex,[]) ->
    L = length(AlertPath),
    receive 
	{imgs,CurrentAlert,[]} ->
	    {NAlertPath,NImageIndex} = 
		if (L > 0) and (CurrentAlert=<0) ->
			alert_finished(AlertPath,Command),{"",0}; true -> {AlertPath,ImageIndex} 
		end,
	    image_writer(Path,Command,NAlertPath,NImageIndex,[]);
	{imgs,CurrentAlert,Imgs} ->
	    {NAlertPath,NImageIndex} = 
		if L == 0 -> {get_current_time_string(Path),0};
		   L > 0 -> if CurrentAlert=<0 -> alert_finished(AlertPath,Command),{"",0}; 
			       true -> {AlertPath,ImageIndex} end
		end,
	    if NAlertPath == [] ->
		    image_writer(Path,Command,NAlertPath,NImageIndex,[]);
	       true ->
		    image_writer(Path,Command,NAlertPath,NImageIndex,lists:reverse(Imgs))
	    end
    end;
image_writer(Path,Command,AlertPath,PrevImageIndex,[Img|Imgs]) ->
    ImageIndex = PrevImageIndex+1,
    if ImageIndex =< 99999 ->
	    FileName = new_image_name(AlertPath,ImageIndex),
	    case file:write_file(FileName,Img,[write,binary]) of
		ok ->
		    ok;
		{error,Reason} ->
		    io:format("Couldn't write to file: ~s (~s)~n",[FileName,Reason])
	    end,
	    image_writer(Path,Command,AlertPath,ImageIndex,Imgs);
       true ->
	    image_writer(Path,Command,"",0,[Img|Imgs])
    end.

alert_finished(AlertPath,Cmd) ->
    NCmd = re:replace(Cmd,"\\$\\{path\}",AlertPath,[{return,list},global]), 
    io:format("Alert finished, executing ~s~n",[NCmd]),
    os:cmd(NCmd).

new_image_name(Path,Index) ->
    S = integer_to_list(Index),
    filename:join([Path,"ipcam-"++string:join(lists:duplicate(5-length(S),"0"),"")++S++".jpg"]).

get_current_time_string(Path) ->
    {{Year,Month,Day},{Hour,Min,_}} = erlang:localtime(),
    Dir = filename:join([Path,add_zero(Year),add_zero(Month),add_zero(Day),add_zero(Hour)++"-"++add_zero(Min)]),
    io:format("Creating Dir: ~s~n",[Dir]),
    filelib:ensure_dir(filename:join([Dir,"ipcam"])),
    Dir.

load_jpeg_data(_,[],Result) ->
    Result;
load_jpeg_data(SkipFrames,[HImgs|TImgs],Result) ->
    case jpeg_nif:load(HImgs) of
	{ok,Img} ->
	    RImgs = if (SkipFrames>0) and TImgs =/= [] -> tl(TImgs); true -> TImgs end,
	    load_jpeg_data(SkipFrames-1,RImgs,[Img|Result]);
	{error,Reason} ->
	    io:format("Couldn't load jpeg (~p)~n",[Reason]),
	    load_jpeg_data(SkipFrames,TImgs,Result)
    end.

get_compare_result(Camera,Debug,Processor,Comparators,Motion,Alert,LastAlert,Processes,LastTime,CAcc,CAccCount,Result) when Comparators == 0 ->
    CR = (Result/Processes)*100, 
    NewTime = os:timestamp(),
    TimeDiffV = time_diff(LastTime,NewTime), TimeDiff = TimeDiffV >= 1.0,
    C = if TimeDiff -> (CR/TimeDiffV+CAcc); true -> (CR+CAcc) end,
    NewLastTime = if TimeDiff -> NewTime; true -> LastTime end,
    Cf = round_prec(C/(CAccCount+1)),
    NewAlert = if TimeDiff and (Cf>=Motion) -> Alert; LastAlert =< 0 -> 0; TimeDiff -> LastAlert-TimeDiffV; true -> LastAlert end,
    if TimeDiff and Debug -> io:format("Comparing Result[~s]: ~p Current Alert:(~p)~n",[Camera,Cf,LastAlert]); true -> ok end,
    if LastAlert>0 -> Processor ! {alert,NewAlert}; true -> ok end,
    {NewCAcc,NewCAccCount} = if TimeDiff -> {CR*(TimeDiffV-1.0),0}; true -> {C,CAccCount+1} end, 
    get_compare_result(Camera,Debug,Processor,Processes,Motion,Alert,NewAlert,Processes,NewLastTime,NewCAcc,NewCAccCount,0);
get_compare_result(Camera,Debug,Processor,Comparators,Motion,Alert,LastAlert,Processes,LastTime,CAcc,CAccCount,Result) ->
    receive
	C ->
	    get_compare_result(Camera,Debug,Processor,Comparators-1,Motion,Alert,LastAlert,Processes,LastTime,CAcc,CAccCount,Result+C)
    end.

send_to_comparator(_,[],_,_) ->
    ok;
send_to_comparator(ImgData,[Comparator|Comparators],Pos,Size) ->
    Comparator ! {ImgData,trunc(Pos),trunc((Pos+1)-Size)},
    send_to_comparator(ImgData,Comparators,Pos-Size,Size).

comparator(Processor,Step,Min) ->
    receive
	{ImgData,StartPos,MinPos} ->
	    Processor ! compare_frames(ImgData,StartPos,MinPos,Step,Min,0),
	    comparator(Processor,Step,Min)
    end.

connect(Host,Port,Path,Frames,Processor) ->
    case gen_tcp:connect(Host,Port, [binary, {packet, raw}, {active, false}]) of 
	{ok, Socket} ->
	    io:format("Connected to ~s~n", [Host]),
	    case gen_tcp:send(Socket,io_lib:format("GET ~s HTTP/1.0\r\n\r\n",[Path])) of
		ok -> 	    
		    case get_boundary(Socket) of
			{ok, Boundary,Rest} ->
			    if Boundary =/= <<>> ->
				    start_capture(Socket,Boundary,Frames,Rest,Processor);
			       true ->
				    io:message("No boundry received from (~p:~p)~n",[Host,Port])
			    end;
			{error,_,_} ->
			    io:message("No boundry received from (~p:~p)~n",[Host,Port])
		    end;
		{error,Reason} ->
		    io:format("Send error (~p)~n",[Reason])
	    end,
	    gen_tcp:close(Socket),
	    connect(Host,Port,Path,Frames,Processor);
	{error, Reason} ->
	    io:format("Couldn't connect to ~s (~p)~n", [Host,Reason]),
	    timer:sleep(10000),
	    connect(Host,Port,Path,Frames,Processor)
    end.

start_capture(Socket,Boundary,Frames,Rest,Processor) ->
    case capture_jpeg(Socket,Boundary,Frames,Rest,[]) of
	{ok,Imgs,Rest2} ->
	    Processor ! {imgs,Imgs},
	    start_capture(Socket,Boundary,Frames,Rest2,Processor);
	{error,_,_} -> ok
    end.

capture_jpeg(_,_,Frames,Rest,Result) when Frames == 0 ->
    {ok,Result,Rest};
capture_jpeg(Socket,Boundary,Frames,Rest,Result) ->
    case get_jpeg(Socket,Boundary,Rest) of
	{ok,Img,R2} ->
	    capture_jpeg(Socket,Boundary,Frames-1,R2,[Img|Result]);
	{error,_,_} ->
	    {error,Result,Rest}
    end.

get_jpeg(Socket,Boundary,Rest) ->
    case recv_till(Socket,Boundary,Rest) of
	{ok,_,R} ->
	    case recv_till(Socket,<<"\r\n\r\n">>,R) of 
		{ok,_,R2} ->
		    recv_till(Socket,<<"\r\n\r\n">>,R2);
		{error,Bi,Ri} -> {error,Bi,Ri}
	    end;
	{error,Bi2,Ri2} -> {error,Bi2,Ri2}
    end.

get_boundary(Socket) ->
    get_boundary(Socket,<<>>).
get_boundary(Socket,Rest) ->
    {ok,_,R} = recv_till(Socket,<<"boundary=">>,Rest),
    recv_line(Socket,R).
    
recv_line(Socket,Rest) ->
    recv_till(Socket,<<10>>,Rest).

recv_till(Socket,LastChar,Bs) ->
    case gen_tcp:recv(Socket, 0, 30000) of
        {ok, Bp} ->
	    B = <<Bs/binary,Bp/binary>>,
	    case binary:match(B,[LastChar]) of	    
	       nomatch ->
	   	    recv_till(Socket, LastChar, B);
	       M ->
		    {I,_} = M,
		    Ms = binary:part(B,0,I),
		    Mp = binary:part(B,I+byte_size(LastChar),byte_size(B)-I-byte_size(LastChar)),
		    {ok, Ms, Mp}
	    end;
        {error, Reason} ->
	    io:format("recv error ~p~n",[Reason]),
            {error,Bs,<<>>}
    end.
    
% Compare a list of frames to each other and return a coefficient of pixel change
compare_frames([_],_,_,_,_,Acc) ->
    Acc;
compare_frames([Img|Rest],Pos,MinPos,Step,Min,Acc) ->
    {_,_,B1} = Img,
    {_,_,B2} = hd(Rest),
    C = jpeg_nif:compare(B1,B2,Pos,MinPos,Step,Min),
    compare_frames(Rest,Pos,MinPos,Step,Min,C+Acc).

%% Some utility functions
time_diff( {AM,AS,AU}, {BM,BS,BU}) -> 
    ((BM-AM) * 1000000) + (BS-AS) + ((BU-AU)/1000000).

round_prec(V) ->
    round_prec(V,2).
round_prec(V,P) ->
    Pi = P*10,
    trunc(V*Pi)/Pi.

add_zero(I) ->
    S = integer_to_list(I),
    if length(S) > 1 -> S; true -> "0"++S end.

