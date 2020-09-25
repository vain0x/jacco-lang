use std::collections::HashMap;

pub(crate) struct JsonArrayPush<'a, 'p> {
    parent: &'p mut JsonArray<'a>,
}

impl<'a, 'p> JsonArrayPush<'a, 'p> {
    pub(crate) fn value(self, value: JsonValue<'a>) -> &'p mut JsonArray<'a> {
        self.parent.0.push(value);
        self.parent
    }

    pub(crate) fn number(self, value: f64) -> &'p mut JsonArray<'a> {
        self.parent.0.push(JsonValue::Number(value));
        self.parent
    }

    pub(crate) fn string(self, value: &'a str) -> &'p mut JsonArray<'a> {
        self.parent.0.push(JsonValue::String(value));
        self.parent
    }
}

#[derive(Default)]
pub(crate) struct JsonArray<'a>(pub(crate) Vec<JsonValue<'a>>);

impl<'a> JsonArray<'a> {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn from_iter(iter: impl IntoIterator<Item = JsonValue<'a>>) -> Self {
        Self(iter.into_iter().collect())
    }

    pub(crate) fn get(&self, i: usize) -> &JsonValue<'a> {
        &self.0[i]
    }

    pub(crate) fn as_slice(&self) -> &[JsonValue<'a>] {
        self.0.as_slice()
    }

    pub(crate) fn push<'p>(&'p mut self) -> JsonArrayPush<'a, 'p> {
        JsonArrayPush { parent: self }
    }

    pub(crate) fn into_value(self) -> JsonValue<'a> {
        JsonValue::Array(self)
    }
}

pub(crate) struct JsonObjectInsert<'a, 'p> {
    parent: &'p mut JsonObject<'a>,
    key: &'a str,
}

impl<'a, 'p> JsonObjectInsert<'a, 'p> {
    pub(crate) fn value(self, value: JsonValue<'a>) -> &'p mut JsonObject<'a> {
        self.parent.0.insert(self.key, value);
        self.parent
    }

    pub(crate) fn boolean(self, value: bool) -> &'p mut JsonObject<'a> {
        self.parent.0.insert(self.key, JsonValue::Boolean(value));
        self.parent
    }

    pub(crate) fn number(self, value: f64) -> &'p mut JsonObject<'a> {
        self.parent.0.insert(self.key, JsonValue::Number(value));
        self.parent
    }

    pub(crate) fn string(self, value: &'a str) -> &'p mut JsonObject<'a> {
        self.parent.0.insert(self.key, JsonValue::String(value));
        self.parent
    }
}

#[derive(Default)]
pub(crate) struct JsonObject<'a>(pub(crate) HashMap<&'a str, JsonValue<'a>>);

impl<'a> JsonObject<'a> {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn get(&self, key: &str) -> &JsonValue<'a> {
        &self.0[key]
    }

    pub(crate) fn insert<'p>(&'p mut self, key: &'a str) -> JsonObjectInsert<'a, 'p> {
        JsonObjectInsert { parent: self, key }
    }

    pub(crate) fn into_value(self) -> JsonValue<'a> {
        JsonValue::Object(self)
    }
}

pub(crate) enum JsonValue<'a> {
    Null,
    Boolean(bool),
    Number(f64),
    String(&'a str),
    Array(JsonArray<'a>),
    Object(JsonObject<'a>),
}

impl<'a> JsonValue<'a> {
    pub(crate) fn parse(s: &str, slot: &'a mut tinyjson::JsonValue) -> JsonValue<'a> {
        *slot = tinyjson::JsonParser::new(s.chars()).parse().unwrap();
        Self::from_tinyjson(slot)
    }

    pub(crate) fn of_boolean(&self) -> bool {
        match *self {
            JsonValue::Boolean(value) => value,
            _ => panic!(),
        }
    }

    pub(crate) fn of_number(&self) -> f64 {
        match *self {
            JsonValue::Number(value) => value,
            _ => panic!(),
        }
    }

    pub(crate) fn of_string(&self) -> &str {
        match self {
            JsonValue::String(value) => value,
            _ => panic!(),
        }
    }

    pub(crate) fn of_array(&self) -> &JsonArray<'a> {
        match self {
            JsonValue::Array(array) => array,
            _ => panic!(),
        }
    }

    pub(crate) fn of_object(&self) -> &JsonObject<'a> {
        match self {
            JsonValue::Object(object) => object,
            _ => panic!(),
        }
    }

    fn from_tinyjson(value: &'a tinyjson::JsonValue) -> JsonValue<'a> {
        match value {
            tinyjson::JsonValue::Null => JsonValue::Null,
            tinyjson::JsonValue::Boolean(value) => JsonValue::Boolean(*value),
            tinyjson::JsonValue::Number(value) => JsonValue::Number(*value),
            tinyjson::JsonValue::String(value) => JsonValue::String(&value),
            tinyjson::JsonValue::Array(vec) => JsonValue::Array(JsonArray(
                vec.iter()
                    .map(|item| JsonValue::from_tinyjson(item))
                    .collect(),
            )),
            tinyjson::JsonValue::Object(map) => JsonValue::Object(JsonObject(
                map.iter()
                    .map(|(key, value)| (key.as_str(), JsonValue::from_tinyjson(value)))
                    .collect(),
            )),
        }
    }

    fn to_tinyjson(&self) -> tinyjson::JsonValue {
        match self {
            JsonValue::Null => tinyjson::JsonValue::Null,
            JsonValue::Boolean(value) => tinyjson::JsonValue::Boolean(*value),
            JsonValue::Number(value) => tinyjson::JsonValue::Number(*value),
            JsonValue::String(value) => tinyjson::JsonValue::String(value.to_string()),
            JsonValue::Array(JsonArray(array)) => {
                tinyjson::JsonValue::Array(array.iter().map(|item| item.to_tinyjson()).collect())
            }
            JsonValue::Object(JsonObject(map)) => tinyjson::JsonValue::Object(
                map.iter()
                    .map(|(key, value)| (key.to_string(), value.to_tinyjson()))
                    .collect(),
            ),
        }
    }

    pub(crate) fn stringify(&self) -> String {
        tinyjson::stringify(&self.to_tinyjson()).unwrap()
    }
}
