(*
 * Copyright 2022 Vincent Chan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

type 'a value = {
  data: 'a;
  left: int;
  right: int;
}

type 'a node = {
  node_left: int;
  node_right: int;
  mutable spec: 'a node_spec;
}

and 'a node_spec =
  | Leaf of 'a value
  | Branch of ('a node * 'a node)  (* left, right *)

and 'a children = {
  left_node: 'a node;
  right_node: 'a node;
}

let create_root_node data =
  {
    node_left = data.left;
    node_right = data.right;
    spec = Leaf data;
  }

let rec insert_value root_node _val =
  match root_node.spec with
  | Leaf leaf_value -> ( (* divide node *)
    if leaf_value.left = _val.left && leaf_value.right = _val.right then (  (* replace it *)
      root_node.spec <- Leaf _val;  (* mutate with new value *)
      root_node
    ) else if leaf_value.left = _val.left then (
      let left_leaf = {
        node_left = _val.left;
        node_right = _val.right;
        spec = Leaf _val;
      } in
      let right_leaf = {
        node_left = _val.right + 1;
        node_right = root_node.node_right;
        spec = Leaf leaf_value;
      } in
      let branch_node = {
        node_left = root_node.node_left;
        node_right = root_node.node_right;
        spec = Branch(left_leaf, right_leaf);
      } in
      branch_node
    ) else if leaf_value.right = _val.right then (
      let left_leaf = {
        node_left = root_node.node_left;
        node_right = _val.left - 1;
        spec = Leaf leaf_value;
      } in
      let right_leaf = {
        node_left = _val.left;
        node_right = _val.right;
        spec = Leaf _val;
      } in
      let branch_node = {
        node_left = root_node.node_left;
        node_right = root_node.node_right;
        spec = Branch(left_leaf, right_leaf);
      } in
      branch_node
    ) else (
      let left_leaf = {
        node_left = root_node.node_left;
        node_right = _val.left - 1;
        spec = Leaf leaf_value;
      } in
      let right_leaf = {
        node_left = _val.left;
        node_right = _val.right;
        spec = Leaf _val;
      } in
      let branch_node = {
        node_left = root_node.node_left;
        node_right = _val.right;
        spec = Branch(left_leaf, right_leaf);
      } in
      let right_leaf' = {
        node_left = _val.right + 1;
        node_right = root_node.node_right;
        spec = Leaf leaf_value;
      } in
      root_node.spec <- Branch(branch_node, right_leaf');
      root_node
    )
  )

  | Branch(left, right) -> (
    if _val.left >= left.node_left && _val.right <= left.node_right then (
      let insert_back = insert_value left _val in
      root_node.spec <- Branch(insert_back, right);
      root_node
    ) else if _val.left <= left.node_right && _val.right >= right.node_left then (
      let left_value = { _val with
        right = left.node_right;
      } in
      let right_value = { _val with
        left = right.node_left;
      } in
      let insert_left = insert_value left left_value in
      let insert_right = insert_value right right_value in
      root_node.spec <- Branch(insert_left, insert_right);
      root_node
    ) else (
      let insert_back = insert_value right _val in
      root_node.spec <- Branch(left, insert_back);
      root_node
    )
  )

let rec find_value root_node pos =
  match root_node.spec with
  | Branch(left, right) ->
    if pos >= left.node_left && pos <= left.node_right then
      find_value left pos
    else
      find_value right pos

  | Leaf leaf_node -> leaf_node.data

let%test _ =
  let test_node = ref (create_root_node {
    left = 0;
    right = 100;
    data = "black";
  }) in

  test_node := insert_value !test_node {
    left = 10;
    right = 20;
    data = "white";
  };

  test_node := insert_value !test_node {
    left = 70;
    right = 80;
    data = "white";
  };

  let test1 = find_value !test_node 5 in
  let test2 = find_value !test_node 15 in
  let test3 = find_value !test_node 25 in

  test1 = "black" &&
  test2 = "white" &&
  test3 = "black"

let%test _ =
  let test_node = ref (create_root_node {
    left = 0;
    right = 100;
    data = "black";
  }) in

  test_node := insert_value !test_node {
    left = 10;
    right = 20;
    data = "white";
  };

  test_node := insert_value !test_node {
    left = 15;
    right = 20;
    data = "red";
  };

  let test1 = find_value !test_node 20 in

  test1 = "red"
