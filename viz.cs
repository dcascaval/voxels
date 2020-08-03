using Rhino;
using Rhino.Geometry;
using Rhino.DocObjects;
using Rhino.Collections;

using GH_IO;
using GH_IO.Serialization;
using Grasshopper;
using Grasshopper.Kernel;
using Grasshopper.Kernel.Data;
using Grasshopper.Kernel.Types;

using System;
using System.IO;
using System.Xml;
using System.Xml.Linq;
using System.Linq;
using System.Data;
using System.Drawing;
using System.Reflection;
using System.Collections;
using System.Windows.Forms;
using System.Collections.Generic;
using System.Runtime.InteropServices;



/// <summary>
/// This class will be instantiated on demand by the Script component.
/// </summary>
public class Script_Instance : GH_ScriptInstance
{
#region Utility functions
  /// <summary>Print a String to the [Out] Parameter of the Script component.</summary>
  /// <param name="text">String to print.</param>
  private void Print(string text) { /* Implementation hidden. */ }
  /// <summary>Print a formatted String to the [Out] Parameter of the Script component.</summary>
  /// <param name="format">String format.</param>
  /// <param name="args">Formatting parameters.</param>
  private void Print(string format, params object[] args) { /* Implementation hidden. */ }
  /// <summary>Print useful information about an object instance to the [Out] Parameter of the Script component. </summary>
  /// <param name="obj">Object instance to parse.</param>
  private void Reflect(object obj) { /* Implementation hidden. */ }
  /// <summary>Print the signatures of all the overloads of a specific method to the [Out] Parameter of the Script component. </summary>
  /// <param name="obj">Object instance to parse.</param>
  private void Reflect(object obj, string method_name) { /* Implementation hidden. */ }
#endregion

#region Members
  /// <summary>Gets the current Rhino document.</summary>
  private readonly RhinoDoc RhinoDocument;
  /// <summary>Gets the Grasshopper document that owns this script.</summary>
  private readonly GH_Document GrasshopperDocument;
  /// <summary>Gets the Grasshopper script component that owns this script.</summary>
  private readonly IGH_Component Component;
  /// <summary>
  /// Gets the current iteration count. The first call to RunScript() is associated with Iteration==0.
  /// Any subsequent call within the same solution will increment the Iteration count.
  /// </summary>
  private readonly int Iteration;
#endregion

  /// <summary>
  /// This procedure contains the user code. Input parameters are provided as regular arguments,
  /// Output parameters as ref arguments. You don't have to assign output parameters,
  /// they will have a default value.
  /// </summary>
  private void RunScript(string x, Box a, ref object A, ref object B, ref object C, ref object D, ref object E)
  {
    // Rhino.RhinoApp.Write("Parsing\n");
    var vx = mk_vx(x);
    A = vx.Print();
    //   Rhino.RhinoApp.Write("Condensing\n");
    var result = collapse(vx);

    result.SetBoxes();
    var bbox = new BoundingBox(result.boxes.SelectMany(p => new Point3d[]{p.GetCorners()[0],p.GetCorners()[6]}));
    var offset = (Vector3d) bbox.Center;
    var skeleton = new List<Line>();
    for (var k = bbox.Min.Z; k <= bbox.Max.Z; k++) {
      for (var j = bbox.Min.Y; j <= bbox.Max.Y; j++) {
        skeleton.Add(new Line(new Point3d(bbox.Min.X, j, k), new Point3d(bbox.Max.X, j, k)));
      }
      for (var i = bbox.Min.X; i <= bbox.Max.X; i++) {
        skeleton.Add(new Line(new Point3d(i, bbox.Min.Y, k), new Point3d(i, bbox.Max.Y, k)));
      }
    }
    for (var j = bbox.Min.Y; j <= bbox.Max.Y; j++) {
      for (var i = bbox.Min.X; i <= bbox.Max.X; i++) {
        skeleton.Add(new Line(new Point3d(i, j, bbox.Min.Z), new Point3d(i, j, bbox.Max.Z)));
      }
    }
    setSizes(result, result.width / 2, result.height);
    //    Rhino.RhinoApp.Write("Flattening\n");

    var tree = new DataTree<Box>();
    var arr = new List<Line>();
    var lbl = new List<string>();
    var bases = new List<Point3d>();


    flatten(tree, arr, lbl, bases, result, offset);
    A = tree;
    B = arr;
    C = lbl;
    D = bases;
    E = skeleton;
  }

  // <Custom additional code> 
  public enum Op {
    Union = 0,
    Diff = 1,
    XP = 2,
    XN = 3,
    YP = 4,
    YN = 5,
    ZP = 6,
    ZN = 7,
    Base = 8
  }

  public struct VxNode {
    public Op op_type;
    public List<VxNode> args;

    public VxNode(Op op, List<VxNode> in_args) {
      op_type = op;
      args = in_args;
    }

    public string Print() {
      var sub = string.Join(" ", this.args.Select(i => i.Print()).ToArray());
      return string.Join(" ", new string[] {"[", this.op_type.ToString(), sub, "]"});
    }
  }

  void setSizes(BoxNode node, int x, int depth) {
    var z = depth * 5;
    node.SetPosition(new Vector3d(x, 0, z));
    switch (node.op_type) {
      case Op.Union:
      case Op.Diff:
        var x1 = x - node.args[0].RightWidth();
        setSizes(node.args[0], x1, depth - 1);
        var x2 = x + node.args[1].LeftWidth();
        setSizes(node.args[1], x2, depth - 1);
        break;
      case Op.Base:
        return;
      default:
        setSizes(node.args[0], x, depth - 1);
        return;
    }
  }

  static Box moveBox(Box box, Vector3d v) {
    var corners = box.GetCorners();
    var c1 = corners[0]; var c2 = corners[6];
    return new Box(new BoundingBox(c1 + v, c2 + v));
  }

  public class BoxNode {
    public Op op_type;
    public List<Box> boxes;
    public ulong data;
    public List<BoxNode> args;
    public int height;
    public int width;
    public Point3d basePt;

    public BoxNode(Op op, List<BoxNode> in_args, ulong in_data) {
      // RhinoApp.WriteLine(op.ToString() + " " + in_data.ToString());
      op_type = op;
      args = in_args;
      data = in_data;
      height = 0;
      width = 6;
      boxes = new List<Box>();
      basePt = new Point3d(0, 0, 0);
      if (op == Op.Base) return;
      foreach (var arg in in_args) {
        height = Math.Max(height, arg.height);
      }
      height += 1;
      if (op == Op.Union || op == Op.Diff) {
        foreach (var arg in in_args) {
          width += arg.width;
        }
      } else {
        width = args[0].width;
      }
    }

    public void SetBoxes() {
      for (var k = 0; k < 4; k++) {
        for (var j = 0; j < 4; j++) {
          for (var i = 0; i < 4; i++) {
            ulong mask = 0x8000000000000000 >> ((k * 16) + (j * 4) + i);
            if ((data & mask) != 0) {
              boxes.Add(new Box(new BoundingBox(new Point3d(i, j, k), new Point3d(i + 1, j + 1, k + 1))));
            }
          }
        }
      }

      foreach (var arg in args) {
        arg.SetBoxes();
      }
    }

    public void SetPosition(Vector3d v) {
      basePt = new Point3d(v.X, v.Y, v.Z);
      for (var i = 0; i < boxes.Count; i++) {
        boxes[i] = moveBox(boxes[i], v);
      }
    }

    public int RightWidth() {
      switch (op_type) {
        case Op.Union:
        case Op.Diff:
          return args[1].width / 2;
        default:
          return width / 2;
      }
    }

    public int LeftWidth() {
      switch (op_type) {
        case Op.Union:
        case Op.Diff:
          return args[0].width / 2;
        default:
          return width / 2;
      }
    }
  }

  Op op_type(string op) {
    op = op.Trim();
    if (op == "union") return Op.Union;
    if (op == "diff") return Op.Diff;
    if (op == "translateX+") return Op.XP;
    if (op == "translateX-") return Op.XN;
    if (op == "translateY+") return Op.YP;
    if (op == "translateY-") return Op.YN;
    if (op == "translateZ+") return Op.ZP;
    if (op == "translateZ-") return Op.ZN;
    return Op.Base;
  }

  string get_balanced(string ss, int i) {
    //  Rhino.RhinoApp.Write("<b>" + ss + "</b>(" + i.ToString() + ")");
    var start = i;
    if (ss[i] == '(') {
      var bal = 1; i++;
      while (bal != 0) {
        if (ss[i] == '(') bal++;
        if (ss[i] == ')') bal--;
        i++;
      }
      var end = i;
      var result = ss.Substring(start, end - start);
      // Rhino.RhinoApp.Write(" => \"" + result + "\" \n");
      return result;
    } else {
      while (i < ss.Length && !Char.IsWhiteSpace(ss, i)) i++;
      var end = i;
      var result = ss.Substring(start, end - start);
      // Rhino.RhinoApp.Write(" => \"" + result + "\" \n");
      return result;
    }
  }

  VxNode mk_vx(string input) {
    var ss = input.Trim();
    if (ss.Length > 2 && ss[0] == '(' && ss[ss.Length - 1] == ')') {
      ss = ss.Substring(1, ss.Length - 2);
    }
    var i = 0;
    while (i < ss.Length && !Char.IsWhiteSpace(ss, i)) i++;
    Op op = op_type(ss.Substring(0, i));
    // Rhino.RhinoApp.Write(op.ToString() + "\n");
    if (op == Op.Base) {
      return new VxNode(op, new List<VxNode>());
    }

    while (Char.IsWhiteSpace(ss, i)) i++;
    if (op == Op.Diff || op == Op.Union) {
      var a1 = get_balanced(ss, i);
      i += a1.Length;
      while (Char.IsWhiteSpace(ss, i)) i++;
      var a2 = get_balanced(ss, i);
      var v1 = mk_vx(a1);
      var v2 = mk_vx(a2);
      return new VxNode(op, new List<VxNode>{v1,v2});
    }
    var a = get_balanced(ss, i);
    var v = mk_vx(a);
    return new VxNode(op, new List<VxNode>{v});
  }

  BoxNode combine(VxNode input) {
    if (input.op_type == Op.Base || input.op_type == Op.Union || input.op_type == Op.Diff) {
      return collapse(input);
    } else {
      var c = combine(input.args[0]);

      switch (input.op_type) {
        case Op.XN: c.data = c.data >> 1; break;
        case Op.XP: c.data = c.data << 1; break;
        case Op.YN: c.data = c.data >> 4; break;
        case Op.YP: c.data = c.data << 4; break;
        case Op.ZN: c.data = c.data >> 16; break;
        case Op.ZP: c.data = c.data << 16; break;
      }

      return c;
    }
  }

  BoxNode collapse(VxNode input) {
    if (input.op_type == Op.Base) {
      return new BoxNode(Op.Base, new List<BoxNode>(), 0x0000000000000001);
    }
    if (input.op_type == Op.Diff) {
      var c1 = collapse(input.args[0]);
      var c2 = collapse(input.args[1]);
      return new BoxNode(Op.Diff, new List<BoxNode>(){c1,c2}, c1.data & (~c2.data));
    }
    if (input.op_type == Op.Union) {
      var c1 = collapse(input.args[0]);
      var c2 = collapse(input.args[1]);
      return new BoxNode(Op.Union, new List<BoxNode>(){c1,c2}, c1.data | c2.data);
    }
    return combine(input);
  }

  void flatten(
    DataTree<Box> tree,
    List<Line> arr,
    List<string> lbl,
    List<Point3d> bases,
    BoxNode node,
  Vector3d offset) {

    if (node.op_type == Op.Base) {
      var p = tree.BranchCount;
      if (node.boxes.Count > 0) {
        tree.Add(node.boxes[0], new GH_Path(p));
      }
    }
    if (node.op_type == Op.Union || node.op_type == Op.Diff) {
      flatten(tree, arr, lbl, bases, node.args[0], offset);
      flatten(tree, arr, lbl, bases, node.args[1], offset);
      var p = tree.BranchCount;
      tree.AddRange(node.boxes, new GH_Path(p));
    }

    foreach (var arg in node.args) {
      var l = new Line(node.basePt + offset, arg.basePt + offset);
      arr.Add(l);
      bases.Add(node.basePt + offset);
      lbl.Add(node.op_type.ToString());
    }
  }

  // </Custom additional code> 
}