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

(*f iteri_n *)
let iteri_n f n = for i=0 to n-1 do f i done

(*f fold_left_i_n *)
let fold_left_i_n f acc n =
  let rec loop acc i =
    if (i>=n) then acc else loop (f acc i) (i+1)
  in
  loop acc 0

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


type t_ba_float32s = (float, Bigarray.float32_elt,        Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint16s  = (int,   Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

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

(*a Ogl_obj arrays *)
(*m OOVnc_type - vertex/normal/color type with 9 floats per coord *)
module OOVnc_type =
struct
  let fpc = 9 (* 9 floats per coordinate - vertex, normal, color/material coord *)
end

(*m OOVnc - extendable arrays of coordinates and indices *)
module OOVnc =
struct
  include Obj_arrays.Ogl_obj_arrays(OOVnc_type)

  let normal_in_wall dx0 dy0 dx1 dy1 =
    let l0 = sqrt (dx0 *. dx0 +. dy0 *. dy0) in
    let l1 = sqrt (dx1 *. dx1 +. dy1 *. dy1) in
    let dxS = (dx0 /. l0) +. (dx1 /. l1) in
    let dyS = (dy0 /. l0) +. (dy1 /. l1) in
    let l = sqrt (dxS *. dxS +. dyS *. dyS) in
    if l < 1E-8 then (0., 0.) else (dxS /. l, dyS /. l)
 
  let add_roof_pt t ?height:(height=0.) x y color =
    let n = t.num_cs in
    t.num_cs <- t.num_cs + 1;
    t.cs.{9*n+0} <- 2. *. x -. 1.;
    t.cs.{9*n+1} <- height;
    t.cs.{9*n+2} <- 1.0-.2. *. y;
    t.cs.{9*n+3} <- 0.;
    t.cs.{9*n+4} <- 1.;
    t.cs.{9*n+5} <- 0.;
    t.cs.{9*n+6} <- color.(0);
    t.cs.{9*n+7} <- color.(1);
    t.cs.{9*n+8} <- color.(2);
    n

  let add_wall_pts t x y xn yn height color =
    let n = t.num_cs in
    t.num_cs <- t.num_cs + 2;
    t.cs.{9*n+0} <- 2. *. x -. 1.;
    t.cs.{9*n+1} <- 0.;
    t.cs.{9*n+2} <- 1.0 -. 2. *. y;
    t.cs.{9*n+3} <- xn;
    t.cs.{9*n+4} <- 0.;
    t.cs.{9*n+5} <- yn;
    t.cs.{9*n+6} <- color.(0);
    t.cs.{9*n+7} <- color.(1);
    t.cs.{9*n+8} <- color.(2);
    t.cs.{9*n+9} <- 2. *. x -. 1.;
    t.cs.{9*n+10} <- height;
    t.cs.{9*n+11} <- 1.0 -. 2.*.y;
    t.cs.{9*n+12} <- xn;
    t.cs.{9*n+13} <- 0.;
    t.cs.{9*n+14} <- yn;
    t.cs.{9*n+15} <- color.(0);
    t.cs.{9*n+16} <- color.(1);
    t.cs.{9*n+17} <- color.(2);
    n
end

(*a Geometry things *)
(*c geometry_base *)
class virtual geometry_base oovnc_t color =
object (self)

  method add_index   i   = OOVnc.add_index   oovnc_t i
  method add_roof_pt ?height:(height=0.) x y       = OOVnc.add_roof_pt oovnc_t ~height:height x y color
  method add_wall_pts ?height:(height=0.) x y xn yn = OOVnc.add_wall_pts oovnc_t x y xn yn height color
  method index           = OOVnc.object_index oovnc_t
  method coord           = OOVnc.object_coord oovnc_t
  method add_strip i n   = OOVnc.add_strip   oovnc_t i n
  method add_points i n  = OOVnc.add_points   oovnc_t i n

  (* 4 points, we add 0 3 1 2
     5 points, we add 0 4 1 3 2
    For 3 points we want 0 1 2
    For 4 points we want 0 1 3 2
    For 5 points we want 0 1 4 2 3

    This is N-N, 0+1, N-1, 0+2, etc; i.e. +-(i+1)/2

    Now it is 0 1 N-1 2 N-2 ...
    e.g. N=7 it goes 0 1 6 2 5 3 4
     *)
  method build_polygon_strip ?height:(height=0.) (coords:t_ba_float32s) num_pts =
    let index = self # index in
    let pt0   = self # coord in
    iteri_n (fun i-> self # add_roof_pt ~height:height coords.{2*i+0} coords.{2*i+1}) num_pts;
    iteri_n (fun i ->
      (*let strip_i = if ((i land 1)!=0) then (num_pts-1-(i/2)) else (i/2) in*)
      let strip_i = if (i=0) then 0 else if ((i land 1)=0) then (num_pts-(i/2)) else (1+i/2) in
      self # add_index (pt0+strip_i)) num_pts;
    self # add_strip index num_pts

  method build_wall (coords:t_ba_float32s) num_pts height =
    let build_wall_segment i =
      let next_i = (i+1) mod num_pts in
      let x0 = coords.{2*i+0} in
      let y0 = coords.{2*i+1} in
      let x1 = coords.{2*next_i+0} in
      let y1 = coords.{2*next_i+1} in
      let xn = y1 -. y0 in
      let yn = x0 -. x1 in
      self # add_wall_pts ~height:height  x0 y0 xn yn; (* Adds two wall points *)
      self # add_wall_pts ~height:height  x1 y1 xn yn; (* Adds two wall points *)
      ()
    in
    let index = self # index in
    let pt0   = self # coord in
    iteri_n build_wall_segment num_pts;
    iteri_n (fun i -> self # add_index (pt0 + 4*i + 1);
                      self # add_index (pt0 + 4*i + 3);
                      self # add_index (pt0 + 4*i + 0);
                      self # add_index (pt0 + 4*i + 2);
            ) num_pts;
    self # add_strip index (num_pts*4)

  method virtual analyze : Feature.t -> Geometry.t -> (int * int )
  method virtual build   : Feature.t -> Geometry.t -> unit
end

(*c geometry_point *)
class geometry_point oovnc_t color =
object (self)
  inherit geometry_base oovnc_t color as super
  val mutable coords = None;
  val mutable num_pts = 0;
  method analyze feature geometry =
    coords  <- Some (Geometry.coords geometry);
    num_pts <- (Bigarray.Array1.dim (Option.get coords))/2;
    (num_pts, num_pts)
  method build feature geometry =
    let coords = Option.get coords in
    let index = self # index in
    iteri_n (fun i-> ignore (self # add_index ( self # add_roof_pt coords.{2*i+0} coords.{2*i+1} ))) num_pts;
    self # add_points index num_pts

end

(*c geometry_convex_polygon *)
class geometry_convex_polygon oovnc_t color =
object (self)
  inherit geometry_base oovnc_t color as super

  method analyze feature geometry =
    let coords = Geometry.coords geometry in
    let num_pts = (Bigarray.Array1.dim coords)/2 in
    (num_pts,num_pts)

  method build feature geometry  =
    let coords = Geometry.coords geometry in
    let num_pts = (Bigarray.Array1.dim coords)/2 in
    self # build_polygon_strip coords num_pts
end

(*c geometry_convex_polygon_with_height *)
class geometry_convex_polygon_with_height oovnc_t color height =
object (self)
  inherit geometry_base oovnc_t color as super

  method analyze feature geometry =
    let coords = Geometry.coords geometry in
    let num_pts = (Bigarray.Array1.dim coords)/2 in
    (num_pts*5, (num_pts*4 + num_pts)) (* wall plus roof *)

  method build feature geometry  =
    let coords = Geometry.coords geometry in
    let num_pts = (Bigarray.Array1.dim coords)/2 in
    self # build_polygon_strip ~height:height coords num_pts;
    self # build_wall coords num_pts height;
    ()
end

(*c geometry_polygon *)
class geometry_polygon oovnc_t color =
object (self)
  inherit geometry_base oovnc_t color as super

  val mutable coords = None;
  val mutable num_pts = 0;
  val mutable strip = [];

  method analyze feature geometry =
    coords <- Some (Geometry.coords geometry);
    let steps  = Geometry.steps  geometry in
    num_pts <- 1+((steps.(1) land 0xff0) lsr 4); (* must be moveto n *)
    let mesh = Mesh.Mesh.create (Option.get coords) 0 num_pts in
    let build_okay = Mesh.Mesh.build mesh (1) in
    if (build_okay) then (
      strip <- Mesh.Mesh.make_triangle_strip mesh;
      (num_pts, List.length strip)
    ) else (
      (0, 0)
    )

  method build feature geometry  =
    if (strip!=[]) then (
      let coords = Option.get coords in
      let strip_length = List.length strip in
      let index = self # index in
      let pt0   = self # coord in
      iteri_n (fun i-> self # add_roof_pt coords.{2*i+0} coords.{2*i+1}) num_pts;
      List.iter (fun n -> ignore (self # add_index (pt0+n))) strip;
      self # add_strip index strip_length
    )
end

(*c geometry_polygon_with_height *)
class geometry_polygon_with_height oovnc_t color height =
object (self)
  inherit geometry_base oovnc_t color as super

  val mutable coords = None;
  val mutable num_coords = 0;
  val mutable num_pts = 0;
  val mutable strip = [];

  method analyze feature geometry =
    coords <- Some (Geometry.coords geometry);
    num_coords <- (Bigarray.Array1.dim (Option.get coords))/2;
    let steps  = Geometry.steps  geometry in
    num_pts <- 1+((steps.(1) land 0xff0) lsr 4); (* must be moveto n *)
    let mesh = Mesh.Mesh.create (Option.get coords) 0 num_pts in
    let build_okay = Mesh.Mesh.build mesh (1) in
    if (build_okay) then (
      strip <- Mesh.Mesh.make_triangle_strip mesh;
      (num_coords*4 + num_pts, num_coords*4 + (List.length strip))
    ) else (
      (num_coords*4, num_coords*4)
    )

  method build feature geometry  =
    let coords = Option.get coords in
    self # build_wall coords num_coords height;
    if (strip!=[]) then (
      let strip_length = List.length strip in
      let index = self # index in
      let pt0   = self # coord in
      iteri_n (fun i-> self # add_roof_pt ~height:height coords.{2*i+0} coords.{2*i+1}) num_pts;
      List.iter (fun n -> ignore (self # add_index (pt0+n))) strip;
      self # add_strip index strip_length
    )
end


(*c ogl_obj_tile_layer *)
class ogl_obj_tile_layer tile layer height color =
    object (self)
      inherit Ogl_gui.Obj.ogl_obj as super
    val mutable plot_pts = [];
    val mutable plot_strips = [];
    val mutable angle=0.;
    val vnc_mult = if height=0. then 1 else 2;
      method create_geometry ~offset =
        let feature_filter f n = debug_filter layer f n in
        let oovnc_t = OOVnc.create 1024 1024 10 in
        let feature_create_geometry acc feature =
          if not (feature_filter feature 0) then acc else
          let geometry = Layer.feature_geometry layer feature in
          match Geometry.geom_type geometry with
          | Point         -> let g = ((new geometry_point oovnc_t color) :> geometry_base)          in (g,feature,geometry)::acc
          | ConvexPolygon -> (
            if (height>0.) then (
              let g = ((new geometry_convex_polygon_with_height oovnc_t color (0.01 *. height)) :> geometry_base) in (g,feature,geometry)::acc
            ) else  (
              let g = ((new geometry_convex_polygon oovnc_t color) :> geometry_base) in (g,feature,geometry)::acc
          ))
          | Polygon       -> let g = ((new geometry_polygon oovnc_t color) :> geometry_base)        in (g,feature,geometry)::acc
          | _ -> acc
        in
        let gfgs = Tile.feature_fold layer feature_create_geometry [] in
        let (num_vncs, num_is) = List.fold_left (fun (nv,ni) (gc,f,g) -> let (gnv,gni) = (gc # analyze f g) in (nv+gnv, ni+gni)) (0,0) gfgs in
        Printf.printf "Num vertices %d num indices %d\n" num_vncs num_is;
        OOVnc.ensure oovnc_t num_vncs num_is;
        List.iter (fun (gc,f,g) -> gc # build f g) gfgs;
        plot_pts <- OOVnc.points oovnc_t;
        plot_strips <- OOVnc.strips oovnc_t;
        OOVnc.display oovnc_t;
        Printf.printf "Got %d points to plot and %d strips to plot\n" (List.length plot_pts) (List.length plot_strips);
        self # create_vao [ ( [ (0,3,Gl.float,false,(3*3*4),0);     (* vertices *)
                                  (1,3,Gl.float,false,(3*3*4),(3*4)); (* normals *)
                                  (2,3,Gl.float,false,(3*3*4),(3*4+3*4)); (* colors *)
                                ], OOVnc.cs oovnc_t)
          ];
        self # add_indices_to_vao (OOVnc.is oovnc_t);
        Ok ()
      method draw view_set other_uids =
        light.{0} <- 0.7 *. (sin angle);
        light.{1} <- 0.7 *. (cos angle);
        Gl.point_size 4.0;
        Gl.uniform3fv other_uids.(2) 1 light;
        Gl.bind_vertex_array vao_glid;
        Gl.cull_face Gl.back;
        (*Gl.enable Gl.cull_face_enum;*)
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
    let v_up_required = Atcflib.Vector.(normalize (assign_cross_product3 (assign_cross_product3 v_g v_at_q (copy v_at)) v_at_q (copy v_at))) in
    let v_axis = Atcflib.Vector.(assign_cross_product3 v_up_q v_up_required (copy v_at)) in
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
      let v = self # joystick_axis_value 1 in
      if (v!=0) then self # move_forward ((float (-v)) /. 32768.0 /. 120.);
      let v = self # joystick_axis_value 0 in
      if (v!=0) then self # move_left ((float (-v)) /. 32768.0 /. 120.);
      let v = self # joystick_axis_value 2 in
      if (v!=0) then self # yaw ((float v) /. 32768.0 /. 40.);
      let v = self # joystick_axis_value 3 in
      if (v!=0) then self # pitch ((float v) /. 32768.0 /. 40.);
      if ((self # is_key_down '\'') || (self # is_key_down '/')) then () else (scale := (799. *. !scale +. 10.)/.800.);
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
                     [ ( [(0,3,Gl.float,false,9*4,0); (1,3,Gl.float,false,9*4,3*4); (2,3,Gl.float,false,9*4,6*4)],
                         (ba_floats [| (-1.); 0.; (-1.);   0.; 1.; 0.;  0.1; 0.4; 0.1;
                                        1.; 0.; (-1.);     0.; 1.; 0.;  0.1; 0.4; 0.0;
                                        1.; 0.;  1.;       0.; 1.; 0.;  0.1; 0.5; 0.1;
                                       (-1.); 0.;  1.;     0.; 1.; 0.;  0.1; 0.4; 0.2;
                            |] (* vertices, normals, colors *)
                     ) ) ]
      in
      let axes = new Ogl_gui.Obj.ogl_obj_geometry
                     Gl.lines 6 
                     [| 0; 1; 0; 2; 0; 3; |] (* indices *)
                     [ ( [(0,3,Gl.float,false,9*4,0); (1,3,Gl.float,false,9*4,3*4); (2,3,Gl.float,false,9*4,6*4)],
                         (ba_floats [| 0.; 0.; 0.;   1.; 0.; 0.;  1.0; 1.0; 1.0;
                                       1.; 0.; 0.;   1.; 0.; 0.;  1.0; 0.0; 0.0;
                                       0.; 1.; 0.;   0.; 1.; 0.;  0.0; 1.0; 0.0;
                                       0.; 0.; 1.;   0.; 0.; 1.;  0.0; 0.0; 1.0;
                            |] (* vertices, normals, colors *)
                     ) ) ]
      in
      let objs : Ogl_gui.Obj.ogl_obj list  = obj_of_layers tile [("water",     [|0.2;0.2;0.8;|], 0. );
                                                                 ("landcover", [|0.5;0.5;0.3;|], 0. );
                                                                 ("landuse",   [|0.5;0.5;0.3;|], 0. );
                                                                 ("building",  [|0.6;0.6;0.6;|], 1. );
                                               ] in
(*      let objs = ((new ogl_obj_data) :> Ogl_gui.Obj.ogl_obj) ::[] in (* :: objs in*)*)
      let objs = (axes :> Ogl_gui.Obj.ogl_obj) :: objs @ [(ground :> Ogl_gui.Obj.ogl_obj)] in
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
  let t = Option.get (File.get_tile_opt map  14 (2+2*2*2049) (2+2*2*2746)) in (* 14 8198 10986 is the station *)
  let t = Option.get (File.get_tile_opt map  14 8197 10987) in  (* 14 8197 10987 is the center of town *)
  let t = Option.get (File.get_tile_opt map  13 (8197/2) (10987/2)) in  (* 14 8197 10987 is the center of town *)
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

