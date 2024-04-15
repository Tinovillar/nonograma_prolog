import React from 'react';

function Square({ value, onClick }) {
    return (
        <button className={`border-2 ${value === '#' ? "bg-black" : "bg-white"} h-20 w-20 rounded-md m-1`} onClick={onClick}>
            {value === 'X' ? value : null}
        </button>
    );
}

export default Square;