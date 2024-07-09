import gleam/bool
import gleam/float
import gleam/int

pub type ObjectType {
  ObjTypeFloat(Float)
  ObjTypeInt(Int)
  ObjTypeString(String)
  ObjTypeBool(Bool)
  ObjTypeNil
  None
}

pub type Object {
  Object(ttype: ObjectType)
}

pub fn to_string(obj: Object) -> String {
  case obj.ttype {
    ObjTypeFloat(f) -> float.to_string(f)
    ObjTypeInt(i) -> int.to_string(i)
    ObjTypeString(str) -> str
    ObjTypeBool(b) -> bool.to_string(b)
    ObjTypeNil -> "Nil"
    None -> "NONE"
  }
}

pub fn is_instance_of(obj: Object, other: ObjectType) -> Bool {
  case obj.ttype, other {
    ObjTypeFloat(_), ObjTypeFloat(_) -> True
    ObjTypeInt(_), ObjTypeInt(_) -> True
    ObjTypeString(_), ObjTypeString(_) -> True
    ObjTypeBool(_), ObjTypeBool(_) -> True
    ObjTypeNil, ObjTypeNil -> True
    _, _ -> False
  }
}
