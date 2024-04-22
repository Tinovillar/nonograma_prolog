import React from 'react';

function Square({ value, onClick }) {
    return (
        <button className={`border-2 ${value === '#' ? "bg-black" : "bg-white"} h-12 w-12 rounded-md m-1`} onClick={onClick}>
            {value === 'X' ? value : null}
        </button>
    );
}

export default Square;