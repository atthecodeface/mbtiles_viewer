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
open Mbtiles
module Option   = Batteries.Option
module Tile     = Vector.Tile
module Layer    = Vector.Layer
module Feature  = Vector.Feature
module Value    = Vector.Value
module KeyValue = Vector.KeyValue
module Geometry = Vector.Geometry

(*a Useful functions *)
let stylesheet = Ogl_gui.create_stylesheet ()

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
(*f gl_int_val, gl_with_int - to feed ints and get ints back from ctypes Opengl *)
let ba_int32_1    = Bigarray.(Array1.create int32 c_layout 1)
let ba_int32s len = Bigarray.(Array1.create int32 c_layout len)
let gl_int_val  f   = f ba_int32_1 ; Int32.to_int ba_int32_1.{0}
let gl_with_int f i = ba_int32_1.{0} <- Int32.of_int i; f ba_int32_1

(*c ogl_obj_tile_layer *)
let light = ba_floats [| (0.5); (0.5); (0.71)|]
let debug_n = Array.make 20 0
let debug_filter l f t = true || (
  debug_n.(t) <- debug_n.(t) + 1;
  (debug_n.(t) mod 4) = 0
                       )

(*  (debug_n.(t) mod 200) = 51*)
  (*Layer.feature_kv_map_default l f (fun v->(Value.as_int v)!=(-9)) false "house_number"*)

class ogl_obj_tile_layer tile layer height color =
    object (self)
      inherit Ogl_gui.Obj.ogl_obj as super
    val mutable plot_pts = [];
    val mutable plot_strips = [];
    val mutable angle=0.;
    val vnc_mult = if height=0. then 1 else 2;
      method create_geometry ~offset =
        let feature_filter f n = debug_filter layer f n in
        let feature_analyze_geometry acc feature =
          if not (feature_filter feature 0) then acc else
          let geometry = Layer.feature_geometry layer feature in
          match Geometry.geom_type geometry with
          | Point -> (
            let (num_vnc,num_is)=acc in
            let coords = Geometry.coords geometry in
            let num_pts = (Bigarray.Array1.dim coords)/2 in
            (num_vnc+num_pts,num_is+num_pts)
          )
          | Polygon (* Concave in some sense, but might still go in one strip *)
          | MultiPolygon -> (
            let (num_vnc,num_is)=acc in
            let coords = Geometry.coords geometry in
            let steps  = Geometry.steps  geometry in
            let num_pts = 1+((steps.(1) land 0xff0) lsr 4) in (* must be moveto n *)
            let mesh = Mesh.Mesh.create coords 0 num_pts in
            let build_okay = Mesh.Mesh.build mesh (1) in
            if (build_okay) then (
              let strip = Mesh.Mesh.make_triangle_strip mesh in
              let strip_length = List.length strip in
              (num_vnc+num_pts,num_is+strip_length)
            ) else (
              acc
            )
          )
          | ConvexPolygon  -> (
            let (num_vnc,num_is)=acc in
            let coords = Geometry.coords geometry in
            let num_pts = (Bigarray.Array1.dim coords)/2 in
            if (height!=0.) then (num_vnc+2*num_pts,num_is+3*num_pts+2) else (num_vnc+num_pts,num_is+num_pts)
          )
          | _ -> acc
        in
        let feature_build_geometry acc feature =
          if not (feature_filter feature 1) then acc else
          let geometry = Layer.feature_geometry layer feature in
          let set_pt_2d ba_vncs x y n =
            ba_vncs.{9*n+0} <- (2.*.x)-.1.;
            ba_vncs.{9*n+1} <- 0.;
            ba_vncs.{9*n+2} <- 1.-.(2.*.y);
            ba_vncs.{9*n+3} <- 0.;
            ba_vncs.{9*n+4} <- 1.;
            ba_vncs.{9*n+5} <- 0.;
            ba_vncs.{9*n+6} <- color.(0);
            ba_vncs.{9*n+7} <- color.(1);
            ba_vncs.{9*n+8} <- color.(2)
          in
          let set_pt_2dh ba_vncs x y n =
            ba_vncs.{9*n+0} <- (2.*.x)-.1.;
            ba_vncs.{9*n+1} <- height*.0.01;
            ba_vncs.{9*n+2} <- 1.-.(2.*.y);
            ba_vncs.{9*n+3} <- 1.;
            ba_vncs.{9*n+4} <- 1.;
            ba_vncs.{9*n+5} <- 1.;
            ba_vncs.{9*n+6} <- color.(0);
            ba_vncs.{9*n+7} <- color.(1);
            ba_vncs.{9*n+8} <- color.(2)
          in
          match Geometry.geom_type geometry with
          | Point -> (
            let (ba_vncs,ba_is,num_vnc,num_is,pts,strips)=acc in
            let coords = Geometry.coords geometry in
            let num_pts = (Bigarray.Array1.dim coords)/2 in
            for i=0 to (num_pts-1) do
                let x = coords.{2*i+0} in
                let y = coords.{2*i+1} in
                set_pt_2d ba_vncs x y (i+num_vnc);
                ba_is.{i+num_is} <- i+num_vnc;
            done;
            let new_pts = (num_is,num_pts)::pts in
            (ba_vncs,ba_is,num_vnc+num_pts,num_is+num_pts,new_pts,strips)
          )
          | Polygon (* Concave in some sense, but might still go in one strip *)
          | MultiPolygon -> (
            let (ba_vncs,ba_is,num_vnc,num_is,pts,strips)=acc in
            let coords = Geometry.coords geometry in
            let steps  = Geometry.steps  geometry in
            let num_pts = 1+((steps.(1) land 0xff0) lsr 4) in (* must be moveto n *)
            let mesh = Mesh.Mesh.create coords 0 num_pts in
            let build_okay = Mesh.Mesh.build mesh (1) in
            if (build_okay) then (
              let strip = Mesh.Mesh.make_triangle_strip mesh in
              let strip_length = List.length strip in
              for i=0 to (num_pts-1) do
                let x = coords.{2*i+0} in
                let y = coords.{2*i+1} in
                let n = i+num_vnc in
                (*Printf.printf "coord %d %f %f\n" n (x*.4096.) (y *. 4096.);*)
                set_pt_2d ba_vncs x y n;
              done;
              List.iteri (fun i n -> ba_is.{i+num_is} <- n+num_vnc) strip;
              let new_strips = (num_is,strip_length)::strips in
              (ba_vncs,ba_is,num_vnc+num_pts,num_is+strip_length,pts,new_strips)
            ) else (
              acc
            )
          )
          | ConvexPolygon  -> (
            let (ba_vncs,ba_is,num_vnc,num_is,pts,strips)=acc in
            let coords = Geometry.coords geometry in
            let num_pts = (Bigarray.Array1.dim coords)/2 in
            for i=0 to (num_pts-1) do
                let x = coords.{2*i+0} in
                let y = coords.{2*i+1} in
                let n = i+num_vnc in
                set_pt_2d ba_vncs x y n;
            done;
            for i=0 to (num_pts-1) do
                let strip_i = 
                  if ((i land 1)!=0) then (num_pts-1-(i/2)) else (i/2)
                in
                ba_is.{i+num_is} <- strip_i+num_vnc;
            done;
            if (height!=0.) then (
              for i=0 to (num_pts-1) do
                let x = coords.{2*i+0} in
                let y = coords.{2*i+1} in
                let n = i+num_vnc+num_pts in
                set_pt_2dh ba_vncs x y n;
              done;
              for i=0 to (num_pts-1) do
                ba_is.{num_is+num_pts+2*i+0} <- num_vnc+i;
                ba_is.{num_is+num_pts+2*i+1} <- num_vnc+num_pts+i;
              done;
              ba_is.{num_is+3*num_pts+0} <- num_vnc;
              ba_is.{num_is+3*num_pts+1} <- num_vnc+num_pts;
              (ba_vncs,ba_is,num_vnc+num_pts*2,num_is+3*num_pts+2,pts,((num_is,3*num_pts+2)::strips))
            ) else (
              (ba_vncs,ba_is,num_vnc+num_pts,num_is+num_pts,pts,((num_is,num_pts)::strips))
            )
          )
          | _ -> acc
        in
        let (num_vncs,num_is) = Tile.feature_fold layer feature_analyze_geometry (0,0) in
        let ba_vncs       = ba_float_array (9*num_vncs) in
        let axis_indices  = ba_uint16_array (num_is) in
        Printf.printf "Num vertices %d num indices %d\n" num_vncs num_is;
        let (_,_,_,_,pts,strips) = Tile.feature_fold layer feature_build_geometry (ba_vncs,axis_indices,0,0,[],[]) in
        plot_pts <- pts;
        plot_strips <- strips;
        Printf.printf "Got %d points to plot and %d strips to plot\n" (List.length plot_pts) (List.length plot_strips);
        self # create_vao [ ( [ (0,3,Gl.float,false,(3*3*4),0);     (* vertices *)
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
module Ordint = struct type t=int let compare a b = Pervasives.compare a b end
module Intset=Set.Make(Ordint)
let v_nz = Atcflib.Vector.make3 0. 0. (-1.)
let v_g  = Atcflib.Vector.make3 0. (-1.) 0.
let qadj = Quaternion.make_rijk 0. 0. 0. 0.001
let apply_gravity q v_g v_at scale =
    let v_up_q = Atcflib.Vector.(apply_q q (copy v_g)) in
    let v_at_q = Atcflib.Vector.(apply_q q (copy v_at)) in
    let v_up_required = Atcflib.Vector.(normalize (cross_product3 (cross_product3 v_g v_at_q) v_at_q)) in
    let v_axis = Atcflib.Vector.(cross_product3 v_up_q v_up_required) in
    let s = (-1.) *. (Atcflib.Vector.modulus v_axis) *. scale in
    let c = sqrt (1. -. (s*.s)) in (* why not -? *)
    ignore (Atcflib.Vector.normalize v_axis);
    ignore (Quaternion.(assign_of_rotation v_axis c s qadj));
    let l = Quaternion.(modulus (premultiply qadj q)) in
    ignore (Quaternion.scale (1.0/.l) q);
    ()
class ogl_widget_mbtile_viewer stylesheet name_values =
  object (self)
    inherit Ogl_gui.Widget.ogl_widget_viewer stylesheet name_values as super
    val location = Array.make 3 0.;    

    (*f mouse - handle a mouse action along the action vector *)
    method create app =
      opt_material <- Some (app#get_material "vnc_vertex") ;
Atcflib.Vector.set 1 0.01 center;
    scale := 60.0;
      super#create app

    (*f draw_content *)
    method draw_content view_set transformation =
Atcflib.Vector.set 1 0.008 center;
      if (Option.is_none opt_material) then () else
      begin    
      (*let x = Atcflib.Vector.(assign v_nz center |> apply_q (self # get_direction)) in*)
      (*Printf.printf "x %s\n" (Atcflib.Vector.str x);*)
        let material = (Option.get opt_material) in
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.identity translation);
        ignore (Matrix.set 0 3 (-. (Atcflib.Vector.get center 0)) translation);
        ignore (Matrix.set 1 3 (-. (Atcflib.Vector.get center 1)) translation);
        ignore (Matrix.set 2 3 (-. (Atcflib.Vector.get center 2)) translation);
        ignore (Matrix.assign_m_m rotation translation view);
        let ar_scale = (min (super#get_content_draw_dims).(0) (super#get_content_draw_dims).(1)) *. 0.35 *. !scale in
        ignore (Matrix.(set 1 1 ar_scale (set 0 0 ar_scale (identity tmp))));  (* Make -1/1 fit the width - but do not scale z *)
        ignore (Matrix.assign_m_m tmp view tmp2);  (* Make -1/1 fit the width - but do not scale z *)
        let other_uids = Ogl_gui.View.Ogl_view.set view_set (Some material) transformation in
        Gl.uniform_matrix4fv other_uids.(0) 1 true (Ogl_gui.Utils.ba_of_matrix4 tmp2); (* 0 -> V *)
        Gl.uniform_matrix4fv other_uids.(1) 1 true Ogl_gui.Utils.identity4; (* 1 -> M *)
        List.iter (fun o -> o#draw view_set other_uids) objs;
        Gl.bind_vertex_array 0;
      end

    (*f idle *)
    method idle _ = 
      if self # is_key_down ',' then self#move_forward ((-0.1) /. !scale);
      if self # is_key_down 'l' then self#move_forward (0.1 /. !scale);
      if self # is_key_down 'q' then self#move_left ((-0.01) /. !scale);
      if self # is_key_down 'w' then self#move_left (0.01 /. !scale);
      if self # is_key_down '.' then self#pitch 0.005;
      if self # is_key_down ';' then self#pitch (-0.005);
      if self # is_key_down 'x' then self#yaw 0.005;
      if self # is_key_down 'z' then self#yaw (-0.005);
      if self # is_key_down 's' then self#roll 0.005;
      if self # is_key_down 'a' then self#roll (-0.005);
      if self # is_key_down '\'' then scale := !scale *. 1.05;
      if self # is_key_down '/' then  scale := !scale /. 1.05;
      if ((self # is_key_down '\'') || (self # is_key_down '/')) then () else (scale := (7. *. !scale +. 60.)/.8.);
      if ((self # is_key_down 's') || (self # is_key_down 'a')) then () else (
        let q = self#get_direction in
        apply_gravity q v_g v_nz 0.03
      ) ;
      if self # is_key_down '=' then None else
        (self#request_redraw ; Some 10)

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
let obj_of_layers tile layer_names_colors =
  let acc_layer acc layer_name_color =
    let layer_name, color, height = layer_name_color in
    match (Tile.get_layer tile layer_name) with
    | Some layer -> (((new ogl_obj_tile_layer tile layer height color):>Ogl_gui.Obj.ogl_obj)::acc)
    | _ -> acc
  in
  List.fold_left acc_layer [] layer_names_colors

(*f xml_additions *)
let xml_additions tile = 
[
("mbtile", fun app _ name_values ->
    (
      let ground = new Ogl_gui.Obj.ogl_obj_geometry
                     Gl.triangle_strip 4 
                     [| 0; 1; 3; 2; |] (* indices *)
                     [ ba_floats [| -1.; 0.; -1.;
                        1.; 0.; -1.;
                        1.; 0.; 1.;
                        -1.; 0.; 1.;|]; (* vertices *)
                     ba_floats [| 0.; 1.; 0.;
                        0.; 1.; 0.;
                        0.; 1.; 0.;
                        0.; 1.; 0.;|]; (* normals *)
                     ba_floats [|0.1; 0.4; 0.1;
                                 0.1; 0.4; 0.0;
                                 0.1; 0.5; 0.1;
                                 0.1; 0.4; 0.2;|];] (* 'colors' *)
      in
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
      let objs : Ogl_gui.Obj.ogl_obj list  = obj_of_layers tile [("water",     [|0.2;0.2;0.8;|], 0. );
                                                                 ("landcover", [|0.5;0.5;0.3;|], 0. );
                                                                 ("landuse",   [|0.5;0.5;0.3;|], 0. );
                                                                 ("building",  [|0.6;0.6;0.6;|], 1. );
                                               ] in
(*      let objs = ((new ogl_obj_data) :> Ogl_gui.Obj.ogl_obj) ::[] in (* :: objs in*)*)
      let objs = (axes :> Ogl_gui.Obj.ogl_obj) :: objs @ [(ground :> Ogl_gui.Obj.ogl_obj)]in
      let widget = new ogl_widget_mbtile_viewer app.Ogl_gui.AppBuilder.stylesheet name_values in
      widget#set_objs objs;
      widget#name_value_args name_values;
      Ogl_gui.AppBuilder.add_child app (widget :> Ogl_gui.Types.t_ogl_widget)
    ))
]

(*a Top level *)
(*let _ = Mesh.test_mesh ()*)

let (map, tile) =
  let map = File.create "/Users/gavinprivate/Git/brew/map/2017-07-03_england_cambridgeshire.mbtiles" in
  File.read_all_tiles map;
  let t = Option.get (File.get_tile_opt map  14 8170 (8*1376) ) in (*11 1025 1376   6 32 43;9 256 344 *)
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
  match (Ogl_gui.AppBuilder.create_app_from_xml app_xml stylesheet (xml_additions tile) app_creator) with
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

