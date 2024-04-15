import React from 'react';

function Square({ value, onClick }) {
    return (
        <button className="border-solid border-black" onClick={onClick}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;