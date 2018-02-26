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
 * @file     mesh.ml
 * @brief    Convert polygons into meshes of triangles/strips
 *
 *)

module Option   = Batteries.Option

(*a Useful functions *)
(*f trace - use with trace __POS__ *)
let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

(*f t_ba_float32s *)
type t_ba_float32s = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

(*a Mesh *)
module Mesh = struct
  (*t t structure
    The triangle array is effectively a mapping p0->p1 p2 last
    An entry has non-zero p1 and p2 if the triangle p0 p1 p2 is formed
    When building a mesh for a polygon every triangle (except the last) eliminates
    one polygon vertex from the set. This is p0.
    Hence every point is in precisely one triangle - as we put the last triangle
    points with the same triangle
    last is true if it is the last triangle (i.e. it 'kills' all three points)
 *)
  type t = {
      verbose: bool;
      ba: t_ba_float32s;
      num_pts : int;
      ofs: int;
      chains : (int * int) array;
      triangles : (int * int * bool) array;
      mutable num_triangles : int;
    }

  (*t create *)
  let count = ref 0
  let create ?verbose:(verbose=(!count mod 5000)=438) ba ofs n = 
    count := !count + 1;
    let triangles = Array.make n (0,0,false)  in
    let chains = Array.init n (fun i -> (((i+n-1) mod n),(i+1) mod n)) in
    { ba; ofs; num_pts=n; chains; triangles; num_triangles=0; verbose; }

  (*f display *)
  let display t =
    Printf.printf "Offset %d num triangles %d num pts %d\n" t.ofs t.num_triangles t.num_pts;
    Printf.printf "Chains:\n";
    Array.iteri (fun i (l,r)->Printf.printf "    %d:   %d  .. %d\n" i l r) t.chains;
    Printf.printf "Triangles:\n";
    Array.iteri (fun p0 (p1,p2,last)->Printf.printf "    %d  . %d . %d  (%b)\n" p0 p1 p2 last) t.triangles;
    ()
    
  (*f side_of_line *)
  let side_of_line dxn dyn dxo dyo = compare (dxn *. dyo) (dxo *. dyn)

  (*f try_triangle *)
  let try_triangle t winding n0 n1 dirn =
    (* n0==fst(chains.(n1)), n1=snd(chains.(n0)) *)
    let (pt0, pt1, pt2) = 
      if dirn then (n0, n1, snd t.chains.(n1)) else (fst t.chains.(n0), n0, n1)
    in
    let (x0,y0) = t.ba.{t.ofs+2*pt0+0} , t.ba.{t.ofs+2*pt0+1} in
    let (x1,y1) = t.ba.{t.ofs+2*pt1+0} , t.ba.{t.ofs+2*pt1+1} in
    let (x2,y2) = t.ba.{t.ofs+2*pt2+0} , t.ba.{t.ofs+2*pt2+1} in
    let (dx0, dy0) = x1 -. x0, y1 -. y0 in
    let (dx1, dy1) = x2 -. x1, y2 -. y1 in
    if (side_of_line dx0 dy0 dx1 dy1)=(-winding) then ( (* Tri pt0 pt1 pt2 is OUTSIDE the polygon *)
      (* Printf.printf "Tri pt0 pt1 pt2 %d,%d,%d is OUTSIDE the polygon\n" pt0 pt1 pt2; *)
      None
    ) else ( (* Tri pt0 pt1 pt2 is INSIDE the polygon - check no other polygon vertices are inside *)
      (* Printf.printf "Tri pt0 pt1 pt2 %d,%d,%d is INSIDE the polygon\n" pt0 pt1 pt2; *)
      let (dx2, dy2) = x0 -. x2, y0 -. y2 in
      let pt_in_triangle n =
        (*Printf.printf "Is pt %d in triangle?\n" n;*)
        let (xn,yn) = t.ba.{t.ofs+2*n+0} , t.ba.{t.ofs+2*n+1} in
        let (dxn1, dyn1) = xn -. x1, yn -. y1 in
        (*
        Printf.printf "test %f,%f compared to %f,%f: %f,%f: %f,%f\n" xn yn x0 y0 x1 y1 x2 y2;
        Printf.printf "side_of_line %f,%f (N-P1) compared to %f,%f P1-P0 is %d\n" dxn1 dyn1 dx0 dy0 (side_of_line dx0 dy0 dxn1 dyn1);
        Printf.printf "side_of_line %f,%f (N-P1) compared to %f,%f P2-P1 is %d\n" dxn1 dyn1 dx1 dy1 (side_of_line dx1 dy1 dxn1 dyn1 );
         *)
        if ( ((side_of_line dx0 dy0 dxn1 dyn1)!=winding) ||
               ((side_of_line dx1 dy1 dxn1 dyn1)!=winding) ) then false else (
          let (dxn2, dyn2) = xn -. x2, yn -. y2 in
          (*Printf.printf "side_of_line %f,%f (N-P2) compared to %f,%f P0-P2 is %d\n" dxn2 dyn2 dx2 dy2 (side_of_line dxn2 dyn2 dx2 dy2 );*)
          ((side_of_line dx2 dy2 dxn2 dyn2)!=(-winding))
        )
      in
      let rec triangle_contains_no_vertices nm =
        if (nm==pt0) then true
        else if (pt_in_triangle nm) then false
        else (
          let next_nm = snd t.chains.(nm) in
          triangle_contains_no_vertices next_nm
        )
      in
      if triangle_contains_no_vertices (snd t.chains.(pt2)) then (
        Some (pt0, pt1, pt2)
      ) else (
        None
      )
    )

  (*f add_triangle *)
  let add_triangle t (pt0, pt1, pt2) =
    if (t.verbose) then Printf.printf "Add triangle %d: %d %d %d\n" t.num_triangles pt0 pt1 pt2;
    let left  = fst t.chains.(pt0) in
    let right = snd t.chains.(pt2) in
    let last = (left=pt2) in
    t.chains.(pt1) <- (pt1, pt1);
    t.chains.(pt0) <- (left, pt2);
    t.chains.(pt2) <- (pt0, right);
    t.triangles.(pt1) <- (pt2, pt0,last);
    t.num_triangles <- t.num_triangles + 1;
    if last then (
      t.triangles.(pt0) <- (pt1, pt2, true);
      t.triangles.(pt2) <- (pt0, pt1, true)
    );
    last

  (*f try_to_add_triangle *)
  let try_to_add_triangle t winding n0 n1 dirn =
    let opt_t = try_triangle t winding n0 n1 dirn in
    match opt_t with
    | None ->
       if dirn then
         (n1, snd t.chains.(n1), dirn, false)
       else
         (fst t.chains.(n0), n0, dirn, false)
    | Some (p0, p1, p2) ->
       if (add_triangle t (p0,p1,p2)) then (
         (0, 0, false, true)
       ) else (
         (p0, p2, not dirn, true)
       )

  (*f build_chain *)
  let build_chain t winding n0 =
    let n1 = snd t.chains.(n0) in
    let dirn = true in
    let rec loop cn0 cn1 cdirn loops_since_add =
      let (nn0, nn1, ndirn, added) = try_to_add_triangle t winding cn0 cn1 cdirn in
      let nlsa = if added then 0 else (loops_since_add+1) in
      if (nn0=nn1) then true
      else if (loops_since_add>=t.num_pts) then false
      else (
        loop nn0 nn1 ndirn nlsa
      )
    in
    loop n0 n1 dirn 0

  (*f find_unfinished *)
  let find_unfinished t =
    let rec loop n =
      if (n>=t.num_pts) then -1 else (
        let (l,r) = t.chains.(n) in
        if (l!=r) then n
        else loop (n+1)
      )
    in
    loop 0

  (*f build *)
  let build t winding =
    let rec loop _ =
      let n = find_unfinished t in
      if (n<0) then (
        if (t.verbose) then display t;
        true
      ) else (
        let okay = build_chain t winding n in
        if not okay then false
        else if t.num_pts-2 = t.num_triangles then true
        else loop ()
      )
    in 
    loop ()

  (*f make_triangle_strip
    need to add winding order maintenance (/reversal) in the future

    At this point we have an array of triangles (p0,p1,p2,last)

    We determine how many times each point is used in a triangle.
    We find the least used point whose triangle has not been used yet.
    Start with this triangle, p0 p1 p2; look at the triangle on p1 - it might be p1 p2 x,
    in which case we should go down that path for the strip, otherwise we will go down
    the path of p2.
    So, we will add a triangle (e.g.) p0 p2 p1, then we want to build on this strip with p2 p1 x, and so on.
    Once a strip runs out (we reach a place where a triangle has been used) we restart the algorithm.

    When a triangle other than the last is used it consumes 1 from the number of uses of p1 and p2,
    and it kills p0 (as its triangle has been used). When the 'last' triangle is consumed - this is the triangle
    p0 p1 p2 which is the triangle for ALL THREE of p0 p1 p2, we kill p0 p1 and p2 (as their triangles have been used)

    Note that because of this mechanism the usage can go below 0 - for the 'last' triangle points that are used 'last'
    before they are actually finally used. This is fine. It does mean being careful about finding the 'least used point'.

    As the strip is built (without paying attention to winding order) the points are just prepended.
    When the first triangle of a new part of the strip is added we have to duplicate
    the last point and the first of the triangle
    
  *)
  let make_triangle_strip t =
    let use_of_pt = Array.make t.num_pts 1 in (* all are used in their own *)
    Array.iter (fun (p1,p2,_) -> use_of_pt.(p1) <- use_of_pt.(p1)+1;
                                 use_of_pt.(p2) <- use_of_pt.(p2)+1;) t.triangles;
    let build_strip_from_point strip p =
      let use_triangle p =
        let (p1,p2,last) = t.triangles.(p) in
        if t.verbose then Printf.printf "Use triangle %d %d %d usages %d %d %d\n" p p1 p2 use_of_pt.(p) use_of_pt.(p1) use_of_pt.(p2);
        if last then (
          use_of_pt.(p)  <- 0;(* mark point as used - cannot use its triangle *)
          use_of_pt.(p1) <- 0;(* mark point as used - cannot use its triangle *)
          use_of_pt.(p2) <- 0;(* mark point as used - cannot use its triangle *)
        ) else (
          use_of_pt.(p)  <- 0;(* mark point as used - cannot use its triangle *)
          use_of_pt.(p1) <- use_of_pt.(p1) - 1;
          use_of_pt.(p2) <- use_of_pt.(p2) - 1;
        );
        (p1,p2,last)
      in
      let (p1,p2,last) = use_triangle p in
      let (next_pt,(tp0,tp1,tp2)) =
        if last then (
          (p, (p,p1,p2))
        ) else (
          let (p1_p1, p1_p2, _) = t.triangles.(p1) in
          if t.verbose then Printf.printf "Matching %d %d with %d %d\n" p1 p2 p1_p1 p1_p2;
          if ((p1_p1=p2) || (p1_p2=p2)) then (p1,(p,p1,p2)) else (p2,(p,p2,p1))
        )
      in
      let rec add_triangles strip p np =
        let (tp0, tp1, _) = t.triangles.(p) in
        if t.verbose then Printf.printf "Add triangle %d %d . ? usage %d\n" p np use_of_pt.(p);
        if use_of_pt.(p)<=0 then strip (* point is used up *)
        else if ((np!=tp0) && (np!=tp1)) then strip (* p np x is not triangle on p *)
        else (
          let (p0, p1, last) = use_triangle p in
          let nnp = if (np=p0) then p1 else p0 in
          if last then nnp::strip (* the last triangle is doom - it only links to itself *)
          else ( (* right, so we can try from np... *)
            add_triangles (nnp::strip) np nnp
          )
        )
      in
      let strip = if (strip=[]) then [] else (tp0::(List.hd strip)::strip) in
      (* should do another one if we need to fix winding order *)
      add_triangles (tp2::tp1::tp0::strip) tp1 tp2
    in
    let rec build_strip strip =
      (*if t.verbose then Array.iteri (fun i u-> Printf.printf "Use of %d %d\n" i u) use_of_pt;*)
      let (_,pt,u) = Array.fold_left (fun (i,n,min) u -> if ((min<=0) || ((u>0) && (min>u))) then (i+1,i,u) else (i+1,n,min)) (0,0,0) use_of_pt in
      if u<=0 then strip 
      else build_strip (build_strip_from_point strip pt)
    in
    if t.verbose then display t;
    let strip = List.rev (build_strip []) in
    if t.verbose then Printf.printf "Strip (%d):" (List.length strip);
    if t.verbose then ( List.iter (fun p->Printf.printf "%d:" p) strip; Printf.printf "\n";);
    strip

  (*f All done *)
end

(*a Test *)
let sfmt = Printf.sprintf
let test_mesh _ =
  let ba_floats  fs = Bigarray.(Array1.of_array float32 c_layout fs) in
  let str_opt_t t = match t with | None -> "none"
                                 | Some (a,b,c) -> sfmt "(%d,%d,%d)" a b c
  in
  let mesh = Mesh.create (ba_floats [|0.;0.; 1.;0.; 1.;1.; 0.;1.;|]) 0 4 in
  Mesh.display mesh;
  let t012 = Mesh.try_triangle mesh 1 0 1 true in
  Printf.printf "Try triangle 0 1 2 winding 1 gives %s\n" (str_opt_t t012);
  let t012 = Mesh.try_triangle mesh (-1) 0 1 true in
  Printf.printf "Try triangle 0 1 2 winding -1 gives %s\n" (str_opt_t t012);

  let mesh = Mesh.create (ba_floats [|0.;0.; 4.;-2.; 2.;0.; 4.;2.;|]) 0 4 in
  Mesh.display mesh;
  let t012 = Mesh.try_triangle mesh 1 0 1 true in
  Printf.printf "Try triangle 0 1 2 winding 1 gives %s\n" (str_opt_t t012);
  let t012 = Mesh.try_triangle mesh (-1) 0 1 true in
  Printf.printf "Try triangle 0 1 2 winding -1 gives %s\n" (str_opt_t t012);

  let mesh = Mesh.create (ba_floats [|0.;0.; 4.;-2.; 2.;0.; 4.;2.;|]) 0 4 in
  Mesh.display mesh;
  let t012 = Mesh.try_triangle mesh 1 0 1 false in
  Printf.printf "Try triangle 3 0 1 winding 1 gives %s\n" (str_opt_t t012);
  let t012 = Mesh.try_triangle mesh (-1) 0 1 false in
  Printf.printf "Try triangle 3 0 1 winding -1 gives %s\n" (str_opt_t t012);

  Printf.printf "********************************************************************************\n";
  Printf.printf "Build chain from pt 0 with winding 1 \n";
  let mesh = Mesh.create (ba_floats [|0.;0.; 4.;-2.; 2.;0.; 4.;2.;|]) 0 4 in
  Mesh.display mesh;
  let mesh_ok = Mesh.build_chain mesh 1 0 in
  Printf.printf "Build chain for mesh from pt 0 with winding 1 %b\n" mesh_ok;
  Mesh.display mesh;

  Printf.printf "********************************************************************************\n";
  Printf.printf "Build chain from pt 3 with winding 1 \n";
  let mesh = Mesh.create (ba_floats [|0.;0.; 4.;-2.; 2.;0.; 4.;2.;|]) 0 4 in
  Mesh.display mesh;
  let mesh_ok = Mesh.build_chain mesh 1 3 in
  Printf.printf "Build chain for mesh from pt 3 with winding 1 %b\n" mesh_ok;
  Mesh.display mesh;

  Printf.printf "********************************************************************************\n";
  Printf.printf "Build mesh with winding 1 \n";
  let mesh = Mesh.create (ba_floats [|0.;0.; 4.;-2.; 2.;0.; 4.;2.;|]) 0 4 in
  Mesh.display mesh;
  let mesh_ok = Mesh.build mesh 1 in
  Printf.printf "Build for mesh with winding 1 %b\n" mesh_ok;
  Mesh.display mesh;


  Printf.printf "********************************************************************************\n";
  Printf.printf "Build mesh with winding 1 \n";
  let mesh = Mesh.create (ba_floats [|100.;10.; 0.;10.; 1.;3.; 0.;0.; 4.;0.; 3.;3.; 7.;3.; 6.;0.; 10.;0.; 9.;3.; 13.;3.; 12.;0.; 16.;0.; 15.;3.|]) 0 14 in
  Mesh.display mesh;
  let mesh_ok = Mesh.build mesh 1 in
  Printf.printf "Build for mesh with winding 1 %b\n" mesh_ok;
  Mesh.display mesh;

  Printf.printf "********************************************************************************\n";
  Printf.printf "Build mesh with winding 1 \n";
  let mesh = Mesh.create (ba_floats [|1.;3.; 0.;0.; 4.;0.; 3.;3.; 7.;3.; 6.;0.; 10.;0.; 9.;3.; 13.;3.; 12.;0.; 16.;0.; 15.;3.; 100.;10.; 0.;10.; |]) 0 14 in
  Mesh.display mesh;
  let mesh_ok = Mesh.build mesh 1 in
  Printf.printf "Build for mesh with winding 1 %b\n" mesh_ok;
  Mesh.display mesh;

  Printf.printf "********************************************************************************\n";
  Printf.printf "Build mesh with winding 1 \n";
  let mesh = Mesh.create (ba_floats [|1.;3.; 0.;0.; 4.;0.; 3.;3.; 7.;3.; 6.;0.; 10.;0.; 9.;3.; 13.;3.; 12.;0.; 16.;0.; 15.;3.; 100.;10.; 0.;10.; |]) 0 14 in
  Mesh.display mesh;
  let mesh_ok = Mesh.build mesh 1 in
  Printf.printf "Build for mesh with winding 1 %b\n" mesh_ok;
  Mesh.display mesh;
  Printf.printf "Strip:";
  let strip = Mesh.make_triangle_strip mesh in
  List.iter (fun p->Printf.printf "%d:" p) strip;
  Printf.printf "\n";

  Printf.printf "********************************************************************************\n";
  Printf.printf "Build mesh with winding 1 \n";
  let mesh = Mesh.create ~verbose:true (ba_floats [|
160.; 130.;
137.; 143.;
117.; 107.;
89.; 124.;
111.; 163.;
90.; 175.;
61.; 123.;
79.; 114.;
70.; 98.;
92.; 86.;
73.; 52.;
108.; 33.;
|]) 0 12 in
  Mesh.display mesh;
  let mesh_ok = Mesh.build mesh 1 in
  Printf.printf "Build for mesh with winding 1 %b\n" mesh_ok;
  Mesh.display mesh;
  Printf.printf "Strip:";
  let strip = Mesh.make_triangle_strip mesh in
  List.iter (fun p->Printf.printf "%d:" p) strip;
  Printf.printf "\n";
  (* let x = [|0|].(1) in *)
  ()
    

(*  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
175:                  5             
170:                               
165:                      4         
160:                               
155:                               
150:                               
145:                               
140:                           1    
135:                               
130:                                0
125:                  3              
120:            6                    
115:                7                
110:                                
105:                        2        
100:              8                  
 95:                                
 90:                                
 85:                   9             
 80:                                
 75:                                
 70:                                
 65:                                
 60:                                
 55:                                
 50:                 A              
 45:                                
 40:                                
 35:                                
 30:                       B         


 *)
