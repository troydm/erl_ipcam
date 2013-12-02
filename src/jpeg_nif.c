/* Copyright (c) 2012 Dmitry Geurkov (troydm) d.geurkov@gmail.com

This file is part of erl_ipcam.
ipcam motion detection and mjpeg data gathering software written in Erlang

erl_ipcam is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include <erl_nif.h>
#include <stdio.h>
#include <jpeglib.h>
#include <jerror.h>

#define error(msg) enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env,msg,ERL_NIF_LATIN1))

static ERL_NIF_TERM load_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    ErlNifBinary in,out;
    struct jpeg_decompress_struct cinfo;
    struct jpeg_error_mgr jerr;
    unsigned int width, height;
    
    // Load in
    enif_inspect_binary(env,argv[0],&in);

    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);

    jpeg_mem_src(&cinfo, in.data, in.size);
    jpeg_read_header (&cinfo, TRUE);

    width = cinfo.image_width;
    height = cinfo.image_height;

    enif_alloc_binary(width*height*3,&out);
    
    cinfo.do_block_smoothing = TRUE;
    cinfo.do_fancy_upsampling = TRUE;
    cinfo.out_color_space = JCS_RGB;

    jpeg_start_decompress(&cinfo);

    JSAMPROW rowp[1];
    unsigned long location = 0;

    rowp[0] = (unsigned char*) malloc(cinfo.output_width*cinfo.num_components);
    
    int i = 0;
    while (cinfo.output_scanline < cinfo.output_height){
	jpeg_read_scanlines(&cinfo, rowp, 1);
	for( i=0; i<cinfo.image_width*cinfo.num_components;i++)
	    out.data[location++] = rowp[0][i];
    }

    free(rowp[0]);
	
    jpeg_finish_decompress (&cinfo);
    jpeg_destroy_decompress (&cinfo);
    
    return enif_make_tuple2(env,enif_make_atom(env,"ok"),
			    enif_make_tuple3(env,
					     enif_make_int(env,width),
					     enif_make_int(env,height),
					     enif_make_binary(env, &out)));
}

static ERL_NIF_TERM compare_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    ErlNifBinary b1,b2;
    int pos,minpos,step,min;

    // Load args
    enif_inspect_binary(env,argv[0],&b1);
    enif_inspect_binary(env,argv[1],&b2);
    enif_get_int(env,argv[2],&pos);
    enif_get_int(env,argv[3],&minpos);
    enif_get_int(env,argv[4],&step);
    enif_get_int(env,argv[5],&min);

    double result = 0.0;
    int startpos = pos;

    while(pos>minpos){

	int R = abs(b1.data[pos-2]-b2.data[pos-2]);
	int G = abs(b1.data[pos-1]-b2.data[pos-1]);
	int B = abs(b1.data[pos]-b2.data[pos]);
	int M = R+G+B;
	
	if(M >= min)
	    result += 1;
	
	pos = pos-3*step;
    }

    return enif_make_double(env,result/((startpos-minpos)/step));
}

static ErlNifFunc nif_funcs[] = {
    {"load", 1, load_nif},
    {"compare", 6, compare_nif},
};

ERL_NIF_INIT(jpeg_nif, nif_funcs, NULL, NULL, NULL, NULL)
