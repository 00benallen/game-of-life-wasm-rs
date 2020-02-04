mod utils;

use std::fmt;
use wasm_bindgen::prelude::*;

extern crate js_sys;
extern crate web_sys;
use web_sys::console;

// A macro to provide `println!(..)`-style syntax for `console.log` logging.
macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

pub struct Timer<'a> {
    name: &'a str,
}

impl<'a> Timer<'a> {
    pub fn new(name: &'a str) -> Timer<'a> {
        console::time_with_label(name);
        Timer { name }
    }
}

impl<'a> Drop for Timer<'a> {
    fn drop(&mut self) {
        console::time_end_with_label(self.name);
    }
}

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
#[repr(u8)] // so every cell is 1 byte
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cell {
    Dead = 0,
    Alive = 1,
}

impl Cell {
    pub fn toggle(&mut self) {
        *self = match *self {
            // overwrite self in memory, interesting
            Cell::Alive => Cell::Dead,
            Cell::Dead => Cell::Alive,
        };
    }
}

#[wasm_bindgen]
#[derive(Default)]
pub struct Universe {
    width: u32,
    height: u32,
    cells: Vec<Cell>,
}

/// Private methods not exported to JavaScript
impl Universe {
    fn get_index(&self, row: u32, column: u32) -> usize {
        (row * self.width + column) as usize
    }

    fn get_cell(&self, row: u32, column: u32) -> Cell {
        self.cells[self.get_index(row, column)]
    }

    fn get_cell_ref(&mut self, row: u32, column: u32) -> &mut Cell {
        let idx = self.get_index(row, column);
        &mut self.cells[idx]
    }

    fn get_index_and_cell(&self, row: u32, column: u32) -> (usize, Cell) {
        let idx = self.get_index(row, column);
        (idx, self.cells[idx])
    }

    fn live_neighbour_count(&self, row: u32, column: u32) -> u8 {
        let mut count = 0;
        for delta_row in [self.height - 1, 0, 1].iter().cloned() {
            // iterate through all 9 positions in (row, column's) neighbourhood
            for delta_col in [self.width - 1, 0, 1].iter().cloned() {
                if delta_row == 0 && delta_col == 0 {
                    // we don't care about the middle cell
                    continue;
                }

                let neighbor_row = (row + delta_row) % self.height; // using modulus to account for wraparound on the edges of the world
                let neighbor_col = (column + delta_col) % self.width;
                count += self.get_cell(neighbor_row, neighbor_col) as u8; // live cells are a 1, dead cells are a 0
            }
        }
        count
    }

    pub fn live_neighbour_count_fast(&self, row: u32, column: u32) -> u8 {
        let mut count = 0;

        let north = if row == 0 { self.height - 1 } else { row - 1 };

        let south = if row == self.height - 1 { 0 } else { row + 1 };

        let west = if column == 0 {
            self.width - 1
        } else {
            column - 1
        };

        let east = if column == self.width - 1 {
            0
        } else {
            column + 1
        };

        let nw = self.get_index(north, west);
        count += self.cells[nw] as u8;

        let n = self.get_index(north, column);
        count += self.cells[n] as u8;

        let ne = self.get_index(north, east);
        count += self.cells[ne] as u8;

        let w = self.get_index(row, west);
        count += self.cells[w] as u8;

        let e = self.get_index(row, east);
        count += self.cells[e] as u8;

        let sw = self.get_index(south, west);
        count += self.cells[sw] as u8;

        let s = self.get_index(south, column);
        count += self.cells[s] as u8;

        let se = self.get_index(south, east);
        count += self.cells[se] as u8;

        count
    }

    pub fn get_cells(&self) -> &[Cell] {
        &self.cells
    }

    pub fn set_cells(&mut self, cells: &[(u32, u32)]) {
        for (row, col) in cells.iter().cloned() {
            let idx = self.get_index(row, col);
            self.cells[idx] = Cell::Alive;
        }
    }
}

/// Public methods, exported to JavaScript.
#[wasm_bindgen]
impl Universe {
    pub fn new(fill: Option<Cell>) -> Universe {
        let _timer = Timer::new("Universe::init");
        utils::set_panic_hook(); // better error messages
        let width = 128;
        let height = 128;

        if let Some(f) = fill {
            let cells = (0..width * height).map(|_| f).collect();

            Universe {
                width,
                height,
                cells,
            }
        } else {
            let cells = (0..width * height)
                .map(|_| {
                    if js_sys::Math::random() < 0.5 {
                        Cell::Alive
                    } else {
                        Cell::Dead
                    }
                })
                .collect();

            Universe {
                width,
                height,
                cells,
            }
        }
    }

    pub fn set_width(&mut self, width: u32) {
        self.width = width;
        self.cells = (0..self.width * self.height).map(|_| Cell::Dead).collect();
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn set_height(&mut self, height: u32) {
        self.height = height;
        self.cells = (0..self.width * self.height).map(|_| Cell::Dead).collect();
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn cells(&self) -> *const Cell {
        self.cells.as_ptr()
    }

    pub fn toggle_cell(&mut self, row: u32, column: u32) {
        self.get_cell_ref(row, column).toggle();
        // let idx = self.get_index(row, column);
        // self.cells[idx].toggle();
    }

    pub fn tick(&mut self) {
        let _timer = Timer::new("Universe::tick");
        let mut next = self.cells.clone();

        for row in 0..self.height {
            for col in 0..self.width {
                let idx = self.get_index(row, col);
                let cell = self.cells[idx];
                let live_neighbors = self.live_neighbour_count_fast(row, col);
                let next_cell = match (cell, live_neighbors) {
                    // Rule 1: Any live cell with fewer than two live neighbours
                    // dies, as if caused by underpopulation.
                    (Cell::Alive, x) if x < 2 => Cell::Dead,
                    // Rule 2: Any live cell with two or three live neighbours
                    // lives on to the next generation.
                    (Cell::Alive, 2) | (Cell::Alive, 3) => Cell::Alive,
                    // Rule 3: Any live cell with more than three live
                    // neighbours dies, as if by overpopulation.
                    (Cell::Alive, x) if x > 3 => Cell::Dead,
                    // Rule 4: Any dead cell with exactly three live neighbours
                    // becomes a live cell, as if by reproduction.
                    (Cell::Dead, 3) => Cell::Alive,
                    // All other cells remain in the same state.
                    (otherwise, _) => otherwise,
                };
                next[idx] = next_cell;
            }
        }
        self.cells = next;
    }

    pub fn render(&self) -> String {
        self.to_string()
    }
}

impl fmt::Display for Universe {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in self.cells.as_slice().chunks(self.width as usize) {
            // this is cool, remember this .as_slice.chunks() pattern
            for &cell in line {
                let symbol = if cell == Cell::Dead { '◻' } else { '◼' };
                write!(f, "{}", symbol)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}
