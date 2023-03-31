use lurk::{field::LurkField, store::Store};

pub enum UserAction {
    CommitPrivate(String),
    Compute(String, String)
}

pub fn transition<F: LurkField>(store: &mut Store<F>, action: UserAction) {
    match action {
        UserAction::CommitPrivate(data) => {
            // Should type check that blob!
            let expr = store.read(&data).unwrap();
            fcomm::evaluate(store, expr, 10000)
        }
        UserAction::Compute(data, _) => {
            let expr = store.read(&data).unwrap();
            fcomm::evaluate(store, expr, 10000)
        }
    }.unwrap();
}

impl TryFrom<Vec<u8>> for UserAction {
    type Error = String;

    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        let slice = value.as_slice();
        let tag = slice.get(0);
        let len = value.len();
        match tag {
            Some(0x00) => {
                let data = &value[1..len];
                let private = String::from_utf8(data.to_vec()).expect("Error converting bytes to string");
                Ok(UserAction::CommitPrivate(private))
            }
            Some(0x01) => {
                let data = &value[1..len];
                let func = String::from_utf8(data.to_vec()).expect("Error converting bytes to string");
                Ok(UserAction::Compute(func, "todo".repeat(1)))
            }
            _ => Err("Deserialization is not respected".to_string()),
        }
    }
}