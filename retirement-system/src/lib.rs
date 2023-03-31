use fcomm::S1;
use lurk::field::LurkField;
use lurk::store::Store;
use tezos_smart_rollup_entrypoint::kernel_entry;
use tezos_smart_rollup_host::runtime::Runtime;
// use tezos_smart_rollup_host::path::OwnedPath;
mod vericomp;
use vericomp::*;

fn execute<Host: Runtime, F: LurkField>(host: &mut Host, store: &mut Store<F>) {
    // Reading the inbox of things to do.
    let input = host.read_input();

    match input {
        Err(_) | Ok(None) => {},
        Ok(Some(msg)) => {
            host.write_debug("Received message\n");

            // Kernel messages start with 0x00 and user messages start with 0x01
            let data = msg.as_ref();
            match data {
                [0x00, ..] => {
                    host.write_debug("Message received from the kernel\n");
                    execute(host, store)
                }
                [0x01, ..] => {
                    host.write_debug("User message received\n");
                    let user_message: Vec<u8> = data[1..data.len()].to_vec();
                    let user_message = UserAction::try_from(user_message);
                    match user_message {
                        Ok(user_message) => {
                            transition(store, user_message);
                            execute(host, store)
                        }
                        Err(_) => execute(host, store),
                    }
                }
                _ => execute(host, store)
            }
        }
    }
}

pub fn entry<Host: Runtime>(host: &mut Host) {
    // let counter_path: OwnedPath = "/counter".as_bytes().to_vec().try_into().unwrap();
    let store = &mut Store::<S1>::default();
    host.write_debug("Hello Kernel\n");
    execute(host, store);
    // let _ = Runtime::store_write(host, &counter_path, &counter, 0);
}

kernel_entry!(entry);