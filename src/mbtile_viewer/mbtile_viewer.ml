(** Copyright (C) 2018,  Gavin J Stark.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @file     mbtile_viewer.ml
 * @brief    View MBtile in ogl_gui
 *
 *)

open Sdl_ogl_gui
open Atcflib
open Tgl4
open Ogl_gui
open Mbtiles
module Option   = Batteries.Option
module Tile     = Vector.Tile
module Layer    = Vector.Layer
module Feature  = Vector.Feature
module Value    = Vector.Value
module KeyValue = Vector.KeyValue
module Geometry = Vector.Geometry

(*a Useful functions *)
let stylesheet = create_stylesheet ()

(*f trace - use with trace __POS__ *)
let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

(*f >>= standard monadic function *)
let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

(*f ba creator functions *)
let ba_float_array   len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint16_array  len = Bigarray.(Array1.create int16_unsigned c_layout len)
let ba_uint16s fs = Bigarray.(Array1.of_array int16_unsigned c_layout fs)
let ba_floats  fs = Bigarray.(Array1.of_array float32 c_layout fs)

(*a Mbtile object etc *)
(*a Top level *)
let play_with_feature tile layer feature =
  let uid = (Feature.uid feature) in
  let geom = (Tile.feature_geometry layer feature) in
  Geometry.display geom;
  if uid<5 then (
    Printf.printf "Feature uid %d (%d kv)\n" uid (Tile.feature_kv_count layer feature);
    let print_name kv =
      if (KeyValue.key_equals kv "name") then (
        let (ks,vs)=KeyValue.strs kv in
        Printf.printf "  %s->%s\n" ks vs
      )
    in
    Tile.feature_kv_iter layer feature print_name;
    Tile.feature_kv_map_default layer feature (fun v->Printf.printf "rank %d\n" (Value.as_int v)) () "rank";
  );
  ()


type t_ba_float32s = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type t_vap = int * int * Tgl4.Gl.enum * bool * int *int (* index size type_ normalized stride offset *)
(*f gl_int_val, gl_with_int - to feed ints and get ints back from ctypes Opengl *)
let ba_int32_1    = Bigarray.(Array1.create int32 c_layout 1)
let ba_int32s len = Bigarray.(Array1.create int32 c_layout len)
let gl_int_val  f   = f ba_int32_1 ; Int32.to_int ba_int32_1.{0}
let gl_with_int f i = ba_int32_1.{0} <- Int32.of_int i; f ba_int32_1

let light = ba_floats [| (0.5); (0.5); (0.71)|]
class ogl_obj_tile_layer tile layer =
    object (self)
      inherit Ogl_gui.Obj.ogl_obj as super
    val mutable plot_pts = [];
    val mutable plot_strips = [];
    val mutable angle=0.;
      (*m create_vao_2 *)
      method create_vao_2 (vabs: ((t_vap list) * Utils.float32_bigarray) list) : unit Utils.ogl_result =

        (* Create and bind vao *)
        vao_glid <- gl_int_val (Gl.gen_vertex_arrays 1);
        Gl.bind_vertex_array vao_glid;

        (* Create vbos for each big_array element *)
        let num_attributes = List.length vabs in
        let vbo_glids      = ba_int32s num_attributes in
        Gl.gen_buffers num_attributes vbo_glids;

        (* Bind each VBO to its big_array element and attrib pointers *)
        let rec acc_bound_vbos buf_i vab acc =
          match vab with
          | [] -> acc
          | (vaps, ba)::vab_tl -> (
            let glid = Int32.to_int (vbo_glids.{buf_i}) in
            let size = Gl.bigarray_byte_size ba in
            Gl.bind_buffer Gl.array_buffer glid;
            Gl.buffer_data Gl.array_buffer size (Some ba) Gl.static_draw;
            let bind_vap (index,size,type_,normalized,stride,offset) =
              Gl.enable_vertex_attrib_array index;
              Gl.vertex_attrib_pointer index size type_ normalized stride (`Offset offset);
            in                                                                              
            List.iter bind_vap vaps;
            acc_bound_vbos (buf_i+1) vab_tl (glid::acc)
          )
        in 
        vertex_data_glids <- (acc_bound_vbos 0 vabs []);
        Ok () 
      method create_geometry ~offset =
        let feature_analyze_geometry acc feature =
          let geometry = Layer.feature_geometry layer feature in
          match Geometry.geom_type geometry with
          | Point -> (
            let (num_vnc,num_is)=acc in
            let coords = Geometry.coords geometry in
            let num_pts = (Bigarray.Array1.dim coords)/2 in
            (num_vnc+num_pts,num_is+num_pts)
          )
          | Polygon -> (
            let (num_vnc,num_is)=acc in
            let coords = Geometry.coords geometry in
            let num_pts = (Bigarray.Array1.dim coords)/2 in
            (num_vnc+num_pts,num_is+num_pts+1)
          )
          | _ -> acc
        in
        let feature_build_geometry acc feature =
          let geometry = Layer.feature_geometry layer feature in
          match Geometry.geom_type geometry with
          | Point -> (
            let (ba_vncs,ba_is,num_vnc,num_is,pts,strips)=acc in
            let coords = Geometry.coords geometry in
            let num_pts = (Bigarray.Array1.dim coords)/2 in
            for i=0 to (num_pts-1) do
                let x = coords.{2*i+0} in
                let y = coords.{2*i+1} in
                let n = i+num_vnc in
                ba_vncs.{9*n+0} <- x;
                ba_vncs.{9*n+1} <- 0.;
                ba_vncs.{9*n+2} <- y;
                ba_vncs.{9*n+3} <- 0.;
                ba_vncs.{9*n+4} <- 1.;
                ba_vncs.{9*n+5} <- 0.;
                ba_vncs.{9*n+6} <- 1.;
                ba_vncs.{9*n+7} <- 0.2;
                ba_vncs.{9*n+8} <- 0.2;
                ba_is.{i+num_is} <- i+num_is;
            done;
            let new_pts = (num_is,num_pts)::pts in
            (ba_vncs,ba_is,num_vnc+num_pts,num_is+num_pts,new_pts,strips)
          )
          | Polygon -> (
            let (ba_vncs,ba_is,num_vnc,num_is,pts,strips)=acc in
            let coords = Geometry.coords geometry in
            let num_pts = (Bigarray.Array1.dim coords)/2 in
            for i=0 to (num_pts-1) do
                let x = coords.{2*i+0} in
                let y = coords.{2*i+1} in
                let n = i+num_vnc in
                ba_vncs.{9*n+0} <- x;
                ba_vncs.{9*n+1} <- 0.;
                ba_vncs.{9*n+2} <- y;
                ba_vncs.{9*n+3} <- 0.;
                ba_vncs.{9*n+4} <- 1.;
                ba_vncs.{9*n+5} <- 0.;
                ba_vncs.{9*n+6} <- 0.2;
                ba_vncs.{9*n+7} <- 0.2;
                ba_vncs.{9*n+8} <- 1.;
            done;
            for i=0 to (num_pts-1) do
                let strip_i = 
                  if ((i land 1)!=0) then (num_pts-1-(i/2)) else (i/2)
                in
                ba_is.{i+num_is} <- strip_i+num_is;
            done;
            Printf.printf "Adding strip %d,%d\n" num_is num_pts;
            let new_strips = (num_is,num_pts)::strips in
            (ba_vncs,ba_is,num_vnc+num_pts,num_is+num_pts,pts,new_strips)
          )
          | _ -> acc
        in
        let (num_vncs,num_is) = Tile.feature_fold layer feature_analyze_geometry (0,0) in
        let ba_vncs       = ba_float_array (9*num_vncs) in
        let axis_indices  = ba_uint16_array (num_is) in
        Printf.printf "Num pts %d\n" num_vncs;
        let (_,_,_,_,pts,strips) = Tile.feature_fold layer feature_build_geometry (ba_vncs,axis_indices,0,0,[],[]) in
        plot_pts <- pts;
        plot_strips <- strips;
        Printf.printf "Got %d pts %d strips\n" (List.length plot_pts) (List.length plot_strips);
        self # create_vao_2 [ ( [ (0,3,Gl.float,false,(3*3*4),0);     (* vertices *)
                                  (1,3,Gl.float,false,(3*3*4),(3*4)); (* normals *)
                                  (2,3,Gl.float,false,(3*3*4),(3*4+3*4)); (* colors *)
                                ],ba_vncs)
          ];
        self # add_indices_to_vao axis_indices;
        Ok ()
      method draw view_set other_uids =
        light.{0} <- 0.7 *. (sin angle);
        light.{1} <- 0.7 *. (cos angle);
        Gl.point_size 4.0;
        Gl.uniform3fv other_uids.(2) 1 light;
        Gl.bind_vertex_array vao_glid;
        List.iter (fun (ofs,num)->Gl.draw_elements Gl.points num Gl.unsigned_short (`Offset (ofs*2))) plot_pts;
        List.iter (fun (ofs,num)->Gl.draw_elements Gl.triangle_strip num Gl.unsigned_short (`Offset (ofs*2))) plot_strips;
        Gl.bind_vertex_array 0;
        ()
    end

(*v app_xml *)
let app_xml = "<?xml?><app>
<window width='1000' height='800' dims='100,100,100' fill='3,3,3' border='1,1,1,1,1,1' border_color='0.3,0.3,0.3' align='0,1,0'>
  <grid fill='3,3,3' align='0,0,0' id='main_grid'>
    <grid_span axis='x' weights='1.0,0.0,0.0'/>
    <grid_span axis='y' weights='1.0,0'/>
    <grid_span axis='z'/>
    <grid_element base='0,1,0'>
      <label text='Mbtile' font_size='15' border_color='0.5,0.1,0.1' fill='3,0,0'/>
    </grid_element>
    <grid_element base='0,0,0'>
      <mbtile dims='50,50,100' fill='3,3,3' border='1,1,1,1,1,1' border_color='0.9,0.9,0.9' id='viewer'/>
    </grid_element>
  </grid>
</window>
</app>"

(*c ogl_widget_mbtile_viewer  - viewer widget *)
class ogl_widget_mbtile_viewer stylesheet name_values =
  object (self)
    inherit Ogl_gui.Widget.ogl_widget_viewer stylesheet name_values as super
    (*f mouse - handle a mouse action along the action vector *)
    method create app =
      opt_material <- Some (app#get_material "vnc_vertex") ;
      super#create app

    method mouse action mouse vector options = None
end

(*c ogl_app_mbtile_viewer - viewer app *)
class ogl_app_mbtile_viewer stylesheet ogl_displays : Ogl_gui.Types.t_ogl_app = 
  object (self)
    inherit Ogl_gui.App.ogl_app stylesheet ogl_displays as super
    method create_shaders =
      super#create_shaders >>= 
        fun _ -> (
          let gl_program_desc = Ogl_gui.Program.Gl_program.make_desc "vnc_vertex.glsl" "fragment_vnc_color.glsl" [] ["M"; "V"; "G"; "P"; "L";] in
          self#add_program "vnc_vertex" gl_program_desc >>= fun _ ->
          Ok ()
        )

    method create_materials =
      super#create_materials >>=
        fun _ -> (
          self#add_material "vnc_vertex" "vnc_vertex" [|"V"; "M"; "L"|] >>= fun _ ->
          Ok ()
        )

  (*f button_pressed *)
  method button_pressed widget =
    Printf.printf "Button pressed %s\n%!" (widget#get_id);
     ()
end
    
(*f obj_of_layers *)
let obj_of_layers tile layer_names =
  let acc_layer acc layer_name =
    match (Tile.get_layer tile layer_name) with
    | Some layer -> (((new ogl_obj_tile_layer tile layer):>Ogl_gui.Obj.ogl_obj)::acc)
    | _ -> acc
  in
  List.fold_left acc_layer [] layer_names

(*f xml_additions *)
let xml_additions tile = 
[
("mbtile", fun app _ name_values ->
    (
      let axes = new Ogl_gui.Obj.ogl_obj_geometry
                     Gl.lines 6 
                     [| 0; 1; 0; 2; 0; 3; |] (* indices *)
                     [ ba_floats [| 0.; 0.; 0.;
                        1.; 0.; 0.;
                        0.; 1.; 0.;
                        0.; 0.; 1.;|]; (* vertices *)
                     ba_floats [| 1.; 0.; 0.;
                        1.; 0.; 0.;
                        0.; 1.; 0.;
                        0.; 0.; 1.;|]; (* normals *)
                     ba_floats [|1.0; 1.0; 1.0;
                       1.0; 0.0; 0.0;
                       0.0; 1.0; 0.0;
                       0.0; 0.0; 1.0;|];] (* 'colors' *)
      in
      let objs : Ogl_gui.Obj.ogl_obj list  = obj_of_layers tile ["place";"water"] in
(*      let objs = ((new ogl_obj_data) :> Ogl_gui.Obj.ogl_obj) ::[] in (* :: objs in*)*)
      let objs = (axes :> Ogl_gui.Obj.ogl_obj) :: objs in
      let widget = new ogl_widget_mbtile_viewer app.Ogl_gui.App.Builder.stylesheet name_values in
      widget#set_objs objs;
      widget#name_value_args name_values;
      Ogl_gui.App.Builder.add_child app (widget :> Ogl_gui.Types.t_ogl_widget)
    ))
]

(*a Top level *)
let (map, tile) =
  let map = File.create "/Users/gavinprivate/Git/brew/map/2017-07-03_england_cambridgeshire.mbtiles" in
  File.read_all_tiles map;
  let t = Option.get (File.get_tile_opt map 9 256 344) in (* 6 32 43 *)
  let pbf = File.get_tile_pbf map t in
  let tile = Tile.create () in
  Tile.parse_pbf tile pbf;
  (map, tile)

let main () =
  let exec = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "Usage: %s [OPTION]\nPlots something\nOptions:" exec in
  let options =
    [ ]
  in
  let anon _ = raise (Arg.Bad "no arguments are supported") in
  Arg.parse (Arg.align options) anon usage;

  let app_creator displays = (new ogl_app_mbtile_viewer stylesheet displays) in
  match (Ogl_gui.App.Builder.create_app_from_xml app_xml stylesheet (xml_additions tile) app_creator) with
    None -> 
    (
      Printf.printf "Failed to create app\n"; exit 1
    )
  | Some app ->
     (
       match (Sdl_ogl_gui.run_app ~ogl_root_dir:"." app) with
         Ok () -> exit 0
       | Error msg -> Printf.printf "%s\n" msg; exit 1
     )

let () = main ()

